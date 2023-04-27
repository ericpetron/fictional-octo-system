include Ast
include Modules
module ApplyRule = ApplyRule (Substitution)

(** [parseExpr s] parses [s] into an expression and [parseRule s] parses [s] into a rule. *)
let parseExpr (s : string) : string expr = Lexing.from_string s |> Parser.prog Lexer.read
let parseRule (s : string) : string rule = Lexing.from_string s |> Parser.rule Lexer.read

(* Copied from main.ml*)
let rules = ref []
let add_file filename = let file = open_in filename in
  try (while true; do rules := parseRule (input_line file) :: !rules done)
  with End_of_file -> close_in file

(* Apply Multiple Rules *)
let rec apply_rules exp = function
  | [] -> None
  | (h,name)::t -> (match ApplyRule.apply_rule h exp with
    | None -> apply_rules exp t
    | Some v -> Some (v,name))

(** apply_to_nf [rules] [expr]
    applies the rules in [rules] to [expr] until no more rules can be applied *)
let rec apply_to_nf rules expr =
  match (apply_rules expr (List.combine rules rules)) with
    | None -> []
    | Some (e,d) -> (e,d)::apply_to_nf rules e

let showStep (exp, rule) = "  "^showExpr exp^"\n=   {"^showRule rule^"}\n"
let rec showProof exp = function
  | [] -> "  "^showExpr exp^"\n"
  | (next_exp,rule)::t -> (showStep (exp,rule))^(showProof next_exp t)
let makeProof start_exp = print_endline ((apply_to_nf !rules start_exp |> showProof start_exp)^"Q.E.D.\n")

(* [notNone x] where [x = Some y] returns y.
   Precondition: x is not None. *)
let notNone = function
  | Some x -> x
  | _ -> raise (Arg.Bad "Precondition \"x is not None\" failed.") (* Changed this to _ because of compiler *)
