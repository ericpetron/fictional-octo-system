open Simplifier
(*open Ast
  open Modules *)

(* Parse and print rules. *)
let _ = add_file "../data/rules.ddx"
let _ = List.map print_endline (List.map showRule !rules)

(* =============>---{ Subsitution Module Tests }---<============= *)
let _ = print_endline "\n\nSubsitution Module Tests:"

(* Make aliases to functions in the Subsitution module. *)
let se = Substitution.empty
let si = Substitution.singleton
let cs = Substitution.combine_substitutions
let st = Substitution.substitute
let pass name = print_endline ("Passed \""^name^"\" Test")
let _ = pass "Subsititions Compilation"

(* Create singletons. Empty will be tested later. *)
let sub0 = si "x" (Var "c")
let sub1 = si "a" (Binop (Add, Var "b", Var "c"))
let sub2 = si "b" (Binop (Add, Int 9, Var "c"))
let sub3 = si "c" (Int 3)
let sub4 = si "c" (Int 4)

(* Simple Tests. *)
let _ = assert (st sub0 (Var "x") = Var "c")
let _ = assert (st sub1 (Var "a") = Binop (Add, Var "b", Var "c"))
let _ = pass "Initial Substitution"

(* Failed Combine Substitutions. *)
let c1 = cs (Some sub1) None
let c2 = cs None (Some sub2)
let c3 = cs None (None : string Substitution.substitution option)
let c4 = cs (Some sub3) (Some sub4)
let c5 = cs (Some sub0) c4
let c6 = cs (Some sub1) (Some (si "a" (Var "c")))

(* Assert Results of Failed Combine Substitutions. *)
let _ = assert (None = c1)
let _ = assert (None = c2)
let _ = assert (None = c3)
let _ = assert (None = c4)
let _ = assert (None = c5)
let _ = assert (None = c6)
let _ = pass "Combine None"

(* Assert that combining equal substitutions is fine. *)
let _ = assert (Some sub3 = cs (Some sub3) (Some sub3))
let _ = pass "Combine Equal"

(* Successful Combine Substitutions. *)
let sub01 = cs (Some sub0) (Some sub1)
let sub23 = cs (Some sub2) (Some sub3)
let sub   = cs (sub01) (sub23) |> notNone
let _ = pass "Combine Some"

(* Create an Expression. *)
let exp1 = Fun ("f", [Var "a"; Var "x"; Int 2])

(* Substitute onto Expressions. *)
let exp2 = st sub exp1
let exp3 = st sub exp2
let exp4 = st sub exp3

(* Use the previous function to print our expressions with their expected values. *)
let _ = print_endline "\nTest Substitution on Expressions:"
let _ = print_endline ("exp1: \""^(exp1 |> showExpr)^"\", expected value: f(a, x, 2)")
let _ = print_endline ("exp2: \""^(exp2 |> showExpr)^"\", expected value: f(b+c, c, 2)")
let _ = print_endline ("exp3: \""^(exp3 |> showExpr)^"\", expected value: f((9+c)+3, 3, 2)")
let _ = print_endline ("exp4: \""^(exp4 |> showExpr)^"\", expected value: f((9+3)+3, 3, 2)")

(* Test empty substitutions with the sub_empty function. *)
let sub_empty exp = try Some (st se exp |> ignore) with Not_found -> None

let sub_empty1 = sub_empty exp1
let sub_empty2 = sub_empty exp2
let sub_empty3 = sub_empty exp3
let sub_empty4 = sub_empty exp4

let _ = assert (sub_empty1 = None)
let _ = assert (sub_empty2 = None)
let _ = assert (sub_empty3 = None)
let _ = assert (sub_empty4 = Some ()) (* exp4 has no variables. *)
let _ = pass "Empty Substitutions"


(* =============>---{ Apply Rule Module Tests }---<============= *)
let _ = print_endline "\n\nApply Rule Module Tests:"

(* Create AppRuleT using uncealed ApplyRuleTest so that we can test each
   function inside the cealed ApplyRule module. *)
module AppRuleT = ApplyRuleTest (Substitution)

(* Define a list of expressions with variables in them. *)
let var7 = Var "7"
let ddx_var1 = Ddx ("E", Var "1")
let ddx_var_exp = Ddx ("F", exp2)
let fun_g = Fun("g", [Int 2; Var "a"])
let var_exps = [exp1; exp2; exp3; var7; ddx_var1; ddx_var_exp; fun_g]

(* Define a list of expressions that do not contain variables. *)
let novar_exps = [exp4; Int 2; Ddx ("F", exp4); Fun ("g", [])]

(* Assert that there are vars in var_exps and not in novar_exps. *)
let _ = assert (List.map AppRuleT.noVars var_exps |> List.for_all not)
let _ = assert (List.map AppRuleT.noVars novar_exps |> List.for_all Fun.id)

(* Test the matching function. *)
let _ = print_endline "Test the Matching Function..."
let exp_start = parseExpr "x + (x + y)"
let exp_end = parseExpr "(y+1) + ((y+1) + (y+1))"
let match1 = assert (AppRuleT.matching (parseExpr "(y+1) + (y+1)") exp_end = None)
let match2 = notNone (AppRuleT.matching exp_start exp_end)
let exp_test = st match2 exp_start
let _ = assert (exp_end = exp_test)
let _ = pass "the Matching Function"

(* Test the apply rule top-level function. *)
let _ = print_endline "\nTest the Apply Rule Top-Level Function..."

let result = parseExpr "0+(x+y)" |> AppRuleT.apply_rule_tl (parseRule "0+x=x") |> notNone
let _ = assert (result = parseExpr "(x+y)")
let _ = assert ((parseExpr "x*1" |> AppRuleT.apply_rule_tl (parseRule "x*1=x")) = Some (parseExpr "x"))
let _ = assert (parseExpr "x*1" |> AppRuleT.apply_rule_tl (parseRule "x+0=x") = None)
let _ = pass "the Apply Rule Top-Level Function"

(* Test the apply rule function. Every expression should simplify to 0. *)
let _ = print_endline "\nTest the Apply Rule Function:"
let start_exp1 = parseExpr "(d/dx (x+1)) - 1"
let proof1 = apply_to_nf !rules start_exp1
let _ = print_endline ("Apply Rule Test 1:\n"^showProof start_exp1 proof1)

let start_exp2 = parseExpr "(d/dx (0+1))"
let proof2 = apply_to_nf !rules start_exp2
let _ = print_endline ("Apply Rule Test 2:\n"^showProof start_exp2 proof2)
let _ = pass "the Apply Rule Function"

(* =============>---{ Randomized Expression Tests: Passed Manually }---<============= *)
let _ = print_endline "\n\nRandomized Apply Rule Tests: Passed Manually"

(* We use a set seed, so the proofs are techically not random unless you make reseed true. *)
let reseed = false
let _ = if not reseed then (Random.init 12345;print_endline "Seed set to 12345")
  else print_endline "No Set Seed"

(* Declare small functions and required lists. *)
let identity bop = if (bop=Add || bop=Subt) then Int 0 else Int 1
let bops = [Add;Subt;Mult;Div;Pow]
let names = ["a";"b";"c";"d";"e";"f";"g";"x";"y";"z";"Israel";"Eric";"Riley"]
let rand_elem_arr arr = Random.int (Array.length arr) |> Array.get arr
let rand_elem lst = Array.of_list lst |> rand_elem_arr

(* Create a custom loop for repeated function application. *)
let loop f arg = (* Was rec didn't need to be, e*)
  let rec go acc = function
    | 0 -> []
    | n -> go (f arg::acc) (n-1)
  in go []

(** Function to generate a random expression for testing. *)
let rec rand_exp depth = if reseed then Random.self_init () else ();
  match (if (depth=0) then 0 else Random.int 8) with
    | 0 -> Int (Random.int 256)
    | 1 -> Var (rand_elem names)
    | 2 | 3 -> Fun (rand_elem names, (loop rand_exp (depth-1) (Random.int 6)))
    | 4 | 5 -> Binop (rand_elem bops, rand_exp (depth-1), rand_exp (depth-1))
    | 6 | 7 -> Ddx (rand_elem names, rand_exp (depth-1))
    | x -> raise (Stdlib.Match_failure ("Random.int: Number out of bounds", 0, x))

(** Function to generate a random expression that is hopefully easier to simplify. *)
let rand_exp_simp depth = if reseed then Random.self_init () else ();
  match (if (depth=0) then 0 else Random.int 8) with
    | 0 -> Int 0
    | 1 -> Var (rand_elem names)
    | 2 -> Fun (rand_elem names, (loop rand_exp (depth-1) (Random.int 6)))
    | 3 | 4 | 5 -> let bop = rand_elem bops in Binop (bop, identity bop, rand_exp (depth-1))
    | 6 | 7 -> Ddx (rand_elem names, rand_exp (depth-1))
    | x -> raise (Stdlib.Match_failure ("Random.int: Number out of bounds", 0, x))

let _ = pass "Compilation"
let _ = print_endline "\n\n10x Random Expressions:"
let _ = loop (fun depth -> rand_exp depth |> showExpr |> print_endline) 8 10

let _ = print_endline "\n10x Hopefully Simplifiable Random Expressions:"
let _ = loop (fun depth -> rand_exp_simp depth |> showExpr |> print_endline) 8 10

(* Randomized ApplyRule Testing. *)
let _ = print_endline "\n\n10x Random Proofs:"
let _ = loop (fun depth -> rand_exp depth |> makeProof) 8 10

let _ = print_endline "\n\n10x Hopefully Simplifiable Random Proofs:"
let _ = loop (fun depth -> rand_exp_simp depth |> makeProof) 8 10

(* let _ = print_endline "Test3..."
let _ = assert ((parseExpr "d/dx ((3*x)/4)" |> AppRuleT.apply_rule_tl
                (parseRule "d/dx a/b = ((d/dx a)*b - (d/dx b)*a) / (b^2)"))
      = Some (parseExpr "(((d/dx 3*x)*4)-(d/dx 4)*(3x))/(4^2)")) *)

(* parseExpr "((x+1)^(0+0)) - (x^((3-2)-(0+1)))" *)

(* let _ = raise Stdlib.Out_of_memory (* Your computer sucks, upgrade it to run my code LOL. *) *)