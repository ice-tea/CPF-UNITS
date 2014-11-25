(* camlp4r *)
(* $Id: parserify.mli,v 1.1 2003/07/10 12:28:22 michel Exp $ *)

type spc =
  [ SPCterm of (MLast.patt * option MLast.expr)
  | SPCnterm of MLast.patt and MLast.expr
  | SPCsterm of MLast.patt ]
;

value parser_of_expr :
  MLast.expr ->
    list (list (spc * option MLast.expr) * option MLast.patt * MLast.expr);
