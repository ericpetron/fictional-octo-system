
open Simplifier

let usage_msg = Sys.executable_name ^ " [-verbose] <file1> [<file2>] ... -e <expression>"
let quiet = ref false
let expressions = ref []
(* let rules = ref []
let add_file filename =
   let chan = open_in filename in
   try (while true; do
       rules := parseRule (input_line chan) :: !rules
     done)
   with End_of_file -> close_in chan *)
let add_expression expr = expressions := parseExpr expr :: !expressions
let printExpr e = print_endline (showExpr e)
let printStep (e,r) = print_endline ("= {"^showRule r^"}");print_string "  ";printExpr e
let speclist =
  [("-quiet", Arg.Set quiet, "Hide derivation steps");
   ("-e", Arg.String add_expression, "Expression to simplify (will read expressions from stdin if omitted)")]
let showDeriv expression = 
    match (!quiet, apply_to_nf !rules expression) with
      | (true, []) -> printExpr expression
      | (true, xs) -> printExpr (match List.hd (List.rev xs) with (x,_) -> x)
      | (false, lst) -> print_string "  ";printExpr expression; (let _ = List.rev_map printStep lst in ())


let () =
       Arg.parse speclist add_file usage_msg;
       if !expressions = [] then
          while true; do
            showDeriv (parseExpr (input_line stdin))
          done
        else (let _ = List.map showDeriv !expressions in ())
