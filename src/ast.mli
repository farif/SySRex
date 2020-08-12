
(* AST for Regular Expression with Propositions. *)

type prop = 
    | Top
    | Bot
    | Var of char   
    | Not of prop  
    | And of prop * prop
    | Or  of prop * prop
    | Xor of prop * prop
    | Implies of prop * prop
    | Iff of prop * prop 

type regexp =
  | Empty
  | P of prop
  | Star of regexp 
  | Choice of regexp * regexp
  | Concat of regexp * regexp

(* Evaluating Propostions *)
val eval : string -> int -> prop -> bool 

(* Evaluating Regular Expressions *)
val match_regex : string -> int -> int -> regexp -> bool
