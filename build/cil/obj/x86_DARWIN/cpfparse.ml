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

open Parsing;;
let _ = parse_error;;
# 2 "src/cpf/cpfparse.mly"
    open Cpfannotation

# 21 "src/cpf/cpfparse.ml"
let yytransl_const = [|
  262 (* COMMA *);
  263 (* LPAREN *);
  264 (* RPAREN *);
  265 (* ARROW *);
  266 (* DEREF *);
  267 (* DOT *);
  268 (* END *);
    0|]

let yytransl_block = [|
  257 (* FSYM *);
  258 (* VSYM *);
  259 (* VAR *);
  260 (* UNINTERP *);
  261 (* NUM *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\006\000\006\000\004\000\004\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\003\000\001\000\003\000\001\000\002\000\
\002\000\003\000\003\000\003\000\001\000\001\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\014\000\015\000\013\000\016\000\017\000\000\000\
\000\000\018\000\000\000\002\000\003\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\008\000\000\000\010\000\000\000\
\004\000\012\000\011\000\000\000\006\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\013\000\014\000\025\000"

let yysindex = "\001\000\
\053\255\000\000\000\000\000\000\000\000\000\000\000\000\053\255\
\053\255\000\000\245\254\000\000\000\000\031\255\255\254\039\255\
\000\000\053\255\053\255\053\255\000\000\042\255\000\000\253\254\
\000\000\000\000\000\000\053\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\006\255\000\000\019\255\
\000\000\000\000\000\000\000\000\000\000\006\255\000\000\054\255\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\007\000\000\000\246\255\247\255\244\255"

let yytablesize = 66
let yytable = "\016\000\
\017\000\001\000\028\000\021\000\022\000\019\000\023\000\020\000\
\024\000\026\000\027\000\021\000\022\000\007\000\015\000\029\000\
\000\000\007\000\024\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\000\000\009\000\000\000\009\000\003\000\
\004\000\005\000\006\000\007\000\018\000\008\000\000\000\019\000\
\009\000\020\000\003\000\004\000\005\000\006\000\007\000\019\000\
\008\000\020\000\019\000\009\000\020\000\003\000\004\000\005\000\
\006\000\007\000\000\000\008\000\000\000\005\000\009\000\000\000\
\000\000\005\000"

let yycheck = "\009\000\
\012\001\001\000\006\001\014\000\014\000\009\001\008\001\011\001\
\018\000\019\000\020\000\022\000\022\000\008\001\008\000\028\000\
\255\255\012\001\028\000\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\255\255\010\001\255\255\012\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\255\255\009\001\
\010\001\011\001\001\001\002\001\003\001\004\001\005\001\009\001\
\007\001\011\001\009\001\010\001\011\001\001\001\002\001\003\001\
\004\001\005\001\255\255\007\001\255\255\008\001\010\001\255\255\
\255\255\012\001"

let yynames_const = "\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  ARROW\000\
  DEREF\000\
  DOT\000\
  END\000\
  "

let yynames_block = "\
  FSYM\000\
  VSYM\000\
  VAR\000\
  UNINTERP\000\
  NUM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cpf_annotation_aux) in
    Obj.repr(
# 19 "src/cpf/cpfparse.mly"
                                   ( _1 )
# 122 "src/cpf/cpfparse.ml"
               : Cpfannotation.cpf_annotation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cs_cpf_exp_list) in
    Obj.repr(
# 22 "src/cpf/cpfparse.mly"
                                   ( CpfCSAnn _1 )
# 129 "src/cpf/cpfparse.ml"
               : 'cpf_annotation_aux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cpf_exp_list) in
    Obj.repr(
# 23 "src/cpf/cpfparse.mly"
                                   ( CpfELAnn _1 )
# 136 "src/cpf/cpfparse.ml"
               : 'cpf_annotation_aux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cpf_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cs_cpf_exp_list_aux) in
    Obj.repr(
# 26 "src/cpf/cpfparse.mly"
                                      ( _1 :: _3 )
# 144 "src/cpf/cpfparse.ml"
               : 'cs_cpf_exp_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cpf_exp) in
    Obj.repr(
# 29 "src/cpf/cpfparse.mly"
                                      ( [ _1 ] )
# 151 "src/cpf/cpfparse.ml"
               : 'cs_cpf_exp_list_aux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cpf_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cs_cpf_exp_list_aux) in
    Obj.repr(
# 30 "src/cpf/cpfparse.mly"
                                      ( _1 :: _3 )
# 159 "src/cpf/cpfparse.ml"
               : 'cs_cpf_exp_list_aux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cpf_exp) in
    Obj.repr(
# 33 "src/cpf/cpfparse.mly"
                                      ( [ _1 ] )
# 166 "src/cpf/cpfparse.ml"
               : 'cpf_exp_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cpf_exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cpf_exp_list) in
    Obj.repr(
# 34 "src/cpf/cpfparse.mly"
                                      ( _1 :: _2 )
# 174 "src/cpf/cpfparse.ml"
               : 'cpf_exp_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cpf_exp) in
    Obj.repr(
# 37 "src/cpf/cpfparse.mly"
                                      ( CpfDerefExp _2 )
# 181 "src/cpf/cpfparse.ml"
               : 'cpf_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cpf_annotation_aux) in
    Obj.repr(
# 38 "src/cpf/cpfparse.mly"
                                      ( CpfParenExp _2 )
# 188 "src/cpf/cpfparse.ml"
               : 'cpf_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cpf_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cpf_exp) in
    Obj.repr(
# 39 "src/cpf/cpfparse.mly"
                                      ( CpfDotOffset (_1,_3) )
# 196 "src/cpf/cpfparse.ml"
               : 'cpf_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cpf_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cpf_exp) in
    Obj.repr(
# 40 "src/cpf/cpfparse.mly"
                                      ( CpfArrowOffset (_1,_3) )
# 204 "src/cpf/cpfparse.ml"
               : 'cpf_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "src/cpf/cpfparse.mly"
                                      ( CpfVarExp _1 )
# 211 "src/cpf/cpfparse.ml"
               : 'cpf_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "src/cpf/cpfparse.mly"
                                      ( CpfFSymExp _1 )
# 218 "src/cpf/cpfparse.ml"
               : 'cpf_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 43 "src/cpf/cpfparse.mly"
                                      ( CpfVSymExp _1 )
# 225 "src/cpf/cpfparse.ml"
               : 'cpf_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "src/cpf/cpfparse.mly"
                                      ( CpfUninterp _1 )
# 232 "src/cpf/cpfparse.ml"
               : 'cpf_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "src/cpf/cpfparse.mly"
                                      ( CpfNum _1 )
# 239 "src/cpf/cpfparse.ml"
               : 'cpf_exp))
(* Entry cpf_annotation *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let cpf_annotation (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Cpfannotation.cpf_annotation)
