(* 
  Regular expressions with propostional alphabets.
  alphabet = {0,1}
  (0|1)*0*
  Atomic Porpositions = AP 
  Alphabets = \Sigma = 2^AP 
  
  \alpha = w \in (\Sigma)* -- word over finite set of atomic propositions
 
  alpha_i = w[i] i\in {0,...,n} = Letter in a finite alphabet at any given index.
  
  p = {w | p\in S and S \in \Sigma}

  p1 + p2 = {w | w \in p1 or w \in p2}

  Example:
  
  AP = {a,b}

  \Sigma = {{\empty},{a},{b},{a,b}}
  \Top = \Sigma
  \bot = {{\emptyset}}

  w = {{a}{a,b}{b}{a}} 
  
  w = {a \and \not b, a \or b, b \and \not a, b \and \not a}

  \alpha_0 = w_0 = {a}   = a \and \not b
  \alpha_1 = w_1 = {a,b} = a \or b
  \alpha_2 = w_2 = {b}   = b \and \not a
  \alpha_3 = w_1 = {a,b} = b \and \not a

  accepted_input  = abba
  accepted_input  = aaba
  -------------------
  regex = (a.(a|b).(b.a))*
  regex = (a & ~b).(a V b).(~b & a).(a & ~b)
  -------------------
  s_0 = a     t_0 = a
  s_1 = b     t_1 = a
  s_2 = b     t_2 = b
  s_3 = a     t_3 = a
  -------------------
  regex_i  = (a & ~b).(a V b).(b & ~a).(a & ~b)
  regex_ii = ((a & ~b).(a V b).(b & ~a).(a & ~b))*

*)

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

(* Expression Evaluation *)
let rec eval w i p  = 
    match p with
    | Top -> true
    | Bot -> false
    | Var v -> v = w.[i]  
    | Not x -> not (eval w i x)
    | And (x1, y1) -> (eval w i x1) && (eval w i y1)   
    | Or (x1, y1) ->  (eval w i x1) || (eval w i y1)
    | Xor (x1, y1) -> not ((eval w i x1) = (eval w i y1))
    | Implies (x1, y1) -> (not (eval w i x1)) ||  (eval w i y1) (* x1 \subseteq y1 *)
    | Iff (x1, y1) -> (eval w i x1) = (eval w i y1)

(* Match relation for regular expression over finite word.
   u : string 
   i : int -- Current point
   j : int -- End point
   r : regexp *)

let rec match_regex w i j re_exp = 
        match re_exp with
        | Empty -> j = i
        | P p -> j = i + 1 &&  eval w i p    
        | Choice (p1, p2) -> match_regex w i j p1 || match_regex w i j p2
        (* w,i,j \model p1 concat p2 iff \exists k\in{i,...,j} (w,i,k) \models p1 and (w,k,j) \models p2) *)
        | Concat (p1, p2) -> List.exists (fun k -> match_regex w i k p1 && match_regex w k j p2) (List.init ((j+1)-i) ((+)i))
        (* u[i,j] \model p1 star  iff i = j or \exists k\in{i+1,...,j} (w[i,k] \models p1 and w[k,j] \models p1.star) *)        
        | Star (p) ->  j = i || 
                        List.exists (fun k -> match_regex w i k p && match_regex w k j re_exp) (List.init (j-i) ((+)(i+1))) 
