{ open Parser }

let white = [' ' '\t']+

rule token = parse
| white {token lexbuf} (* skip whitespace *)

(* Propositional Logic *)
| ['a'-'z'] as c { VAR c } 
| 'T' {TOP}
| 'F' {BOT}
| '~' { NOT }
| '&' { AND }
| 'V' { OR }
| 'X' { XOR }
| '>' { IMPLIES }
| '=' { IFF }

| '(' {LPAR}
| ')' {RPAR}

(* Regular Expression *)
| 'E' { EMPTY } 
| '*' { STAR }
| '|' { CHOICE }
| '.' { CONCAT }

| '(' { LPAR }
| ')' { RPAR }

| eof { EOF }
