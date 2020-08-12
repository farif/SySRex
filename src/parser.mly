%{ open Ast %}

%token <char> VAR 
%token TOP BOT NOT AND OR XOR IMPLIES IFF 
%token EMPTY STAR CHOICE CONCAT LPAR RPAR
%token EOF

%left LPAR RPAR
%left AND OR
%left IMPLIES
%left XOR
%left IFF
%left NOT

%left CONCAT 
%left CHOICE
%left STAR

%start main
%type <Ast.regexp> main

%%

main: r = regex EOF { r }

regex : 
   | EMPTY                          { Empty }
   | r = prop                       {P r}   (* Single character *)  
   | LPAR r = regex RPAR            { r }       (* Grouping *)
   | a = regex CHOICE b = regex     { Choice(a, b) }  (* Alternation *)
   | a = regex CONCAT b = regex     { Concat(a,b) }  (* Concatenation *)  
   | a = regex STAR                 { Star(a) }     (* Kleene Star *)  

prop : 
   | TOP    {Top}
   | BOT    {Bot}
   | c = VAR                        { Var c }    
   | NOT p = prop                   { Not(p) }      
   | p1 = prop AND p2 = prop        { And(p1, p2) }
   | p1 = prop OR p2 = prop         { Or(p1, p2) } 
   | p1 = prop XOR p2 = prop        { Xor(p1, p2) } 
   | p1 = prop IMPLIES p2 = prop    { Implies(p1, p2) }
   | p1 = prop IFF p2 = prop        { Iff(p1, p2) }  
   | LPAR p = prop RPAR             {  p }

