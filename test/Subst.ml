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