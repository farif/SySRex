open Ast

let rec format_prop = function
    | Top -> "Top"
    | Bot -> "Bot"
    | Var c -> "Var '" ^ String.make 1 c ^ "'"
    | Not(a) -> "Not("^format_prop a^")"
    | And(a, b) -> "And("^format_prop a^", "^format_prop b^")"
    | Or(a, b) -> "Or("^format_prop a^", "^format_prop b^")"
    | Xor(a, b) -> "Xor("^format_prop a^", "^format_prop b^")"
    | Implies(a, b) -> "Implies("^format_prop a^", "^format_prop b^")"
    | Iff(a, b) -> "Iff("^format_prop a^", "^format_prop b^")"

(* star < concat < choice *)

let rec format_regex = function
    | Empty -> "Empty"
    | P p -> "P ("^format_prop p^")"
    | Star(a) -> "Star("^format_regex a^")"
    | Choice(a, b) -> "Choice("^format_regex a^", "^format_regex b^")"
    | Concat(a, b) -> "Concat("^format_regex a^", "^format_regex b^")"

