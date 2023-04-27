open Ast
module StringMap = Map.Make(String);;

(* Useful toString functions for testing.*)
let showOp = function 
  | Add -> "+"
  | Subt -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Pow -> "^"
let rec showExpr = function
  | Int n -> string_of_int n
  | Var x -> x
  | Fun (f, lst) -> f^"("^String.concat "," (List.map showExpr lst) ^")"
  | Binop (op, left, right) -> showExprParens left ^ showOp op ^ showExprParens right
  | Ddx (x, exp) -> "d/d"^x^" "^showExprParens exp
and showExprParens exp = match exp with 
  | Binop _ -> "("^showExpr exp ^")"
  | Ddx _ -> "("^showExpr exp ^")"
  | _ -> showExpr exp
let showRule (Rule (e1,e2)) = showExpr e1 ^ " = " ^ showExpr e2


module type Substitution = sig
  type 'a substitution

  (**
      combine_substitutions [subst1] [subst2]
      compares the composition of the substitutions [subst1] and [subst2] for compatibility:
      equal variables need to be mapped to the same thing.
      If the composition is compatible, it returns the composition.
  *)
  val combine_substitutions : 'a substitution option -> 'a substitution option -> 'a substitution option
  exception MalformedSubstitution of string

  (**
      substitute [subst] [pat] replaces the variables in [pat] by
      whatever the subtitution [subst] tells them to be.
      If a variable occurs in [pat] that is not in [subst], a NotFound error is raised.
      An occurrence in 'ddx' requires the variable to be a single variable.
      If it is given an expression instead, the MalformedSubstitution error is raised.
  *)
  val substitute : 'a substitution -> string expr -> 'a expr

  (**
      singleton [x] [e] returns a substitution that maps [x] to [e].    
  *)
  val singleton : string -> 'a expr -> 'a substitution

  (** empty returns a substitution that maps nothing.
      *)
  val empty : 'a substitution

  (** for_all allows you to check whether a substitution satisfies a predicate.
      It returns true if the predicate is true for all variables in the substitution.
      This can be used to check if constant-variables are mapped to constants, for example.    
  *)
  val for_all : (string -> 'a expr -> bool) -> 'a substitution -> bool
end

module type ApplyRule = sig
  (** apply_rule [rule] [expr] tries to apply the rule [rule] to the expression [expr].
      If succesful, it returns the rewritten form of [expr], and it returns None otherwise.
      The function apply_some_rule does the same on lists of expressions,
      it applies the rule to precisely one element (if possible). *)
  val apply_rule : string rule -> string expr -> string expr option
end

module Substitution : Substitution = struct
  type 'a substitution = 'a expr StringMap.t

  let empty = StringMap.empty
  let singleton = StringMap.singleton
  let for_all = StringMap.for_all

  exception Conflict
  let merge _ l r = if (l=r) then (Some l) else raise Conflict

  let combine_substitutions sub1_opt sub2_opt =
    match sub1_opt,sub2_opt with
      | Some sub1, Some sub2 -> (try Some (StringMap.union merge sub1 sub2)
                                 with Conflict -> None)
      | _ -> None

  exception MalformedSubstitution of string
  let rec substitute (subst : 'a substitution) : (string expr -> 'a expr) = function
    | Int num -> Int num
    | Var name -> StringMap.find name subst
    | Fun (fun_name, exp_lst) -> Fun (fun_name, List.map (substitute subst) exp_lst)
    | Binop (bop, exp1, exp2) -> Binop (bop, substitute subst exp1, substitute subst exp2)
    | Ddx (var_name, exp) -> (match StringMap.find var_name subst with
      | Var x_name -> Ddx (x_name, substitute subst exp)
      | _ -> let msg = ("Ddx ("^var_name^", exp), exp must be a variable.")
          in raise (MalformedSubstitution msg))
end

(* Create ApplyRuleTest as an uncealed functor for testing. *)
module ApplyRuleTest (Substitution : Substitution) = struct
  (** matching [pattern] [term]
      finds a substitution that can be applied to [pattern] to make
      it equal [term], if such a substitution exists.
      Otherwise, it returns None.
      
    [matching a b = Some s] ==> [substitute s a = b]
    (Exists s2. [matching a b = Some s2]) <=> [substitute s a = b] *)
  let rec matching (start:string expr) (result:'a expr):'a Substitution.substitution option =
    match (start, result) with
      | Int x, Int y when x=y -> Some Substitution.empty
      | Var x, exp -> Some (Substitution.singleton x exp)
      | Fun (name1, exp_lst1), Fun (name2, exp_lst2) when name1=name2 ->
        if not (List.length exp_lst1 = List.length exp_lst2) then None else
          let rec lst_matching = function
            | [], [] -> Some Substitution.empty
            | h1::t1, h2::t2 -> lst_matching (t1,t2) |>
              Substitution.combine_substitutions (matching h1 h2)
            | _ -> (let msg="List Lenths Not Equal", List.length exp_lst1, List.length exp_lst2
                    in raise (Stdlib.Match_failure msg)) (* Should never run. *)
          in lst_matching (exp_lst1, exp_lst2)
      | Binop (bop1, exp1, exp2), Binop (bop2, exp1', exp2') when bop1=bop2 ->
        Substitution.combine_substitutions (matching exp1 exp1') (matching exp2 exp2')
      | Ddx (var1, exp1), Ddx (var2, exp2) -> Substitution.combine_substitutions (matching exp1 exp2) (Some (Substitution.singleton var1 (Var var2)))
      | _ -> (*print_endline ("maching: No case "^showExpr start^" -> "^showExpr result^".");*)None
            (* This is a bit verbose we should only use it for debuging. *)

  (** noVars [e] returns whether there are any variables in [e].
    The purpose of this function is to know if the subexpression
    can be considered to be a constant, i.e. for a rule like 'd/dx c = 0'.
    For that reason, the occurrence of d/dx itself is not considered a variable. *)
  let rec vars : ('a expr -> bool) = function
    | Int _ -> false
    | Var _ -> true
    | Fun (_, lst) -> List.exists vars lst
    | Binop (_, l, r) -> vars l || vars r
    | Ddx (_, exp) -> vars exp
  let noVars x = vars x |> not
  
  (* If sub name starts with c or n, make sure exp has no variables.
     Idea: Require that names starting with v are variables. *)
  let check_substitution : ('a Substitution.substitution -> bool) =
    Substitution.for_all (fun name exp -> let start = String.get name 0 in
                          not (vars exp && (start='c' || start='n')))
  
  (** [if_none f arg opt] returns [f arg] if opt is none. Otherwise, returns opt. *)
  let if_none f arg = function 
    | Some x -> Some x
    | _ -> f arg
  
  (* Typecheck if_none. *)
  let _ : ('a -> 'b option) -> 'a -> 'b option -> 'b option = if_none

  (** [apply_rule_tl [rule] [exp]] tries to apply [rule] to the [exp].
      It returns the rewritten form of [exp] if the rule can be applied.
      This function ignores subexpressions. *)
  let apply_rule_tl (Rule (l,r) : string rule) (exp : string expr) : string expr option =
    (* print_endline ("Call to ARtl -> Rule:"^showRule (Rule (l,r))^", Exp:"^showExpr exp); *)
    match matching l exp with
      | Some sub when check_substitution sub -> Some (Substitution.substitute sub r)
      | _ -> None (* Rule cannot be applied. *)

  (** [apply_rule [rule] [exp]] tries to apply [rule] to the [exp] and
      returns Some rewritten form if successful. Otherwise, it returns none.
      apply_rule does not ignore subexpressions. *)
  let rec apply_rule (rule: string rule) (exp : string expr) : (string expr option) =
    (* print_endline ("Call to AR -> Rule:"^showRule rule^", Exp:"^showExpr exp); *)
    apply_rule_tl rule exp |> if_none (function
      | Int _ | Var _ -> None
      | Fun (_, exp_lst) -> let rec lst_apply = function
        | [] -> None
        | h::t -> apply_rule rule h |> if_none lst_apply t
        in lst_apply exp_lst
      | Binop (bop, left, right) -> (match apply_rule rule left with
        | Some new_left -> Some (Binop (bop, new_left, right))
        | None -> (match apply_rule rule right with
          | Some new_right -> Some (Binop (bop, left, new_right))
          | None -> None))
      | Ddx (_, exp) -> apply_rule rule exp) exp
end

(* Create a sealed ApplyRule for use in the rest of the code. *)
module ApplyRule (Subst : Substitution) : ApplyRule = ApplyRuleTest(Subst)

(* "dune test --profile release" For testing command *)