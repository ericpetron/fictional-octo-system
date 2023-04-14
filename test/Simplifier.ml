open Simplifier

(* Parse and print rules.*)
let _ = add_file "../data/rules.ddx"
let _ = List.map print_endline (List.map showRule !rules)

(* Assume options are not None. *)
exception Impossible
let notNone = function
  | None -> raise Impossible (* Should never happen. *)
  | Some x -> x

(* Create Substitutions. *)
let nothing = Substitution.empty
let sub0 = Substitution.singleton "x" (Var "c")
let sub1 = Substitution.singleton "a" (Binop (Add, Var "b", Var "c"))
let sub2 = Substitution.singleton "b" (Binop (Add, Int 9, Var "c"))
let sub3 = Substitution.singleton "c" (Int 3)

(* Combine Substitutions. *)
let sub01 = Substitution.combine_substitutions (Some sub0) (Some sub1)
let sub23 = Substitution.combine_substitutions (Some sub2) (Some sub3)
let sub = Substitution.combine_substitutions (sub01) (sub23) |> notNone

(* Test Combining with None. *)
let subNone = assert (sub1 = (Substitution.combine_substitutions (Some sub1) None |> notNone))
let noneSub = assert (sub2 = (Substitution.combine_substitutions None (Some sub2) |> notNone))
let noneNone= assert (None = Substitution.combine_substitutions None None)

(* Create an Expression. *)
let exp1 = Fun ("f", [Var("a"); Var "x"; Int 2])

(* Substitute onto Expressions. *)
let exp2 = Substitution.substitute sub exp1
let exp3 = Substitution.substitute sub exp2
let exp4 = Substitution.substitute sub exp3

(* Use the previous function to print our expressions with their expected values. *)
let _ = print_endline ("exp1: \""^(exp1 |> string_exp)^"\", expected value: f(a, x, 2)")
let _ = print_endline ("exp2: \""^(exp2 |> string_exp)^"\", expected value: f(b+c, c, 2)")
let _ = print_endline ("exp3: \""^(exp3 |> string_exp)^"\", expected value: f(9+c+3, 3, 2)")
let _ = print_endline ("exp4: \""^(exp4 |> string_exp)^"\", expected value: f(9+3+3, 3, 2)")