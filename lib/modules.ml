open Ast
module Hashmap = Map.Make(String);;

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
  type 'a substitution = 'a expr Hashmap.t

  let empty : 'a substitution = Hashmap.empty
  let singleton : (string -> 'a expr -> 'a substitution) = Hashmap.singleton
  let for_all : ((string -> 'a expr -> bool) -> 'a substitution -> bool) = Hashmap.for_all

  (*let merge _ exp1_opt exp2_opt : (key -> 'a expr -> 'a expr -> 'a expr option) = match exp1_opt with
    | None -> exp2_opt
    | Some exp1 -> (match exp2_opt with
      | None -> exp1_opt
      | Some exp2 -> if (exp1=exp2) then Some exp1 else None)*)
  let merge _ exp1_opt exp2_opt : (key -> 'a expr -> 'a expr -> 'a expr option) = match (exp1_opt, exp2_opt) with
    | None, x | x, None -> x
    | Some exp1, Some exp2 -> if (exp1=exp2) then Some exp1 else None
  let combine_substitutions ('a substitution option -> 'a substitution option -> 'a substitution option) = Hashmap.union merge

  (** substitute [subst] [pat] replaces the variables in [pat] by
      whatever the subtitution [subst] tells them to be.
      If a variable occurs in [pat] that is not in [subst], a NotFound error is raised.
      An occurrence in 'ddx' requires the variable to be a single variable.
      If it is given an expression instead, the MalformedSubstitution error is raised. *)
  exception MalformedSubstitution of string
  let rec substitute subst : ('a substitution -> string expr -> 'a expr) = function
    | Int num -> Int num
    | Var name -> Hashmap.find name subst
    | Fun (str, exp_lst) -> Fun (str, List.map (substitute subst) exp_lst)
    | Binop (bop, exp1, exp2) -> Binop (bop, substitute subst exp1, substitute subst exp2)
    | Ddx (str, exp) -> (match exp with (* Note: Possible source of errors. Might need to str or only substitute x or something. :/ *)
      | Var name -> Hashmap.find name subst
      | _ -> raise (MalformedSubstitution "Substitution \"Ddx("^str^", expr)\":\n  Expected: \"Var(name)\"\n  Found: ?")) (* TODO: Print expr. *)
end

module ApplyRule (Substitution : Substitution) : ApplyRule = struct
  (** matching [pattern] [term]
      finds a substitution that can be applied to [pattern] to make
      it equal [term], if such a substitution exists.
      Otherwise, it returns None.
      
    [matching a b = Some s] ==> [substitute s a = b]
    (Exists s2. [matching a b = Some s2]) <=> [substitute s a = b] 
    
    TODO: write this function!
    *)

  (** noVars [e] returns whether there are any variables in [e].
    The purpose of this function is to know if the subexpression
       can be considered to be a constant, i.e. for a rule like 'd/dx c = 0'.
    For that reason, the occurrence of d/dx itself is not considered a variable.

    TODO: write this!
    *)
  
  (** To get you started, let's assume all substitutions are okay.
      This is not true, but it will allow you to work on the other parts of the code first.
    *)
  let check_substitution (subst : 'a Substitution.substitution) : bool = true
  
  (** apply_rule_toplevel [rule] [expr]
        tries to apply the rule [rule] to the expression,
        returning the rewritten form if the rule can be applied to the expression as is.
        This function does not try to apply the rule to any subexpressions

        TODO: write this! (Currently it always returns None)
         *)
  let apply_rule_toplevel (Rule (lhs,rhs) : string rule) (expr : string expr)
   = None
  
  (** This is the main work-horse. *)
  let rec apply_rule (rl: string rule) (expr: string expr) : string expr option = None
  
end