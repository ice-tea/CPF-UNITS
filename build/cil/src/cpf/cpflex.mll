{
  open Cpfparse
  exception CpfEndInput

  let uninterpStr = ref ""
}
    
  let cpfIdent = ['a' - 'z' 'A' - 'Z'] ['-' 'a' - 'z' 'A' - 'Z' '0' - '9']*
  let cIdent = ['_' 'a' - 'z' 'A' - 'Z'] ['_' 'a' - 'z' 'A' - 'Z' '0' - '9']*
  let uninterp = [^ ' ' '\t' '(' ')' ',' '-' '*' '.' '@' '$' ';']
  let digit = ['0' - '9']

  rule cpftoken = parse
(* First, tackle various separators *)
    | [' ' '\t']                           { cpftoken lexbuf }
    | '('                                  { LPAREN }
    | ')'                                  { RPAREN }
    | ','                                  { COMMA }

(* Second, variables and annotation function and constant symbols *)
    | '@' (cpfIdent as id)                 { FSYM id }
    | '$' (cpfIdent as id)                 { VSYM id }
    | cIdent as id                         { VAR id }

(* Inserted here: numbers, since we are having conflicts by not *)
(* having these defined -- uninterpreted blocks cannot include - *)
(* since that will swallow -> if used like x->y versus x -> y, *)
(* but then -2 cannot be uninterpreted. *)
  | ('+' digit+) as n                      { NUM n }
  | ('-' digit+) as n                      { NUM n }
  | digit+ as n                            { NUM n }
  | ('+' digit+ '.' digit*) as f           { NUM f }
  | ('-' digit+ '.' digit*) as f           { NUM f }
  | (digit+ '.' digit*) as f               { NUM f }

(* Third, various offset formers in C and dereferencing; we need this *)
(* so we can identify the variable name, versus (for instance) field names *)
(* in structures and unions *)
    | "->"                                 { ARROW }
    | '*'                                  { DEREF }
    | '.'                                  { DOT }

(* Fourth, everything else. We don't interpret any of this, we just read *)
(* it in and spit it back out. Note that we assume some separators are put *)
(* in the annotations, so something like ^2@nothing is one token versus two *)
(* or more. *)
    | ";;"                                 { END }
    | uninterp+ as id                      { UNINTERP id }

(* Finally, end of input *)
    | eof                                  { raise CpfEndInput } 

(* do not modify this function: *)
{ let cpflextest s = cpftoken (Lexing.from_string s) }


