type token =
  | FSYM of (string)
  | VSYM of (string)
  | VAR of (string)
  | UNINTERP of (string)
  | NUM of (string)
  | COMMA
  | LPAREN
  | RPAREN
  | ARROW
  | DEREF
  | DOT
  | END

val cpf_annotation :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cpfannotation.cpf_annotation
