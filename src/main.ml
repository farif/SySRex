(* 
    Goal: Learning minimal regular expressions from finite words. 
*)

(* Read atomic propostions *)

(* Read Regular Expression *)
let () = Printf.printf "Regular Expression: " 
let p = read_line()
let re_exp = Parser.main Lexer.token (Lexing.from_string p) 
let () =  print_endline  ("Expression (AST): " ^ (Util.format_regex re_exp))  

(* Read Input Word *)
let () = Printf.printf "Finite word: "
let w = read_line()
let i = 0
let j = String.length w

(* Check provided word is a model of regex  *)
let r = Ast.match_regex w i j re_exp

let print_result o = match o with
    | true ->  "Accepted" 
    | false -> "Rejected"

let () = print_endline ("Result: "^ (print_result r))
(*  
    Testing Examples: 
    1. a|b.c*
    2. (a . b)*|c 
    3. (a | ~b)*
    4. a|~a.b*
    5. ((a . b)|c)*.b
    6. a*.c|b
    7. a.(b* ) 
    8. (a V b).c 
*)
