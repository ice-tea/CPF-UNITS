open Cpfannotation
module L = Cpflex
module P = Cpfparse
module E = Errormsg

(* Given an input annotation, return the AST for this annotation. If we cannot parse the annotation, *)
(* generate a warning message and just return it as an uninterpreted string. *)
let ann_ast (s:string) = 
  let ast = 
    try 
      P.cpf_annotation L.cpftoken (Lexing.from_string (s ^ ";;"))
    with
      | Failure s' -> (E.warn "Could not parse annotation %s\n" s); CpfCSAnn [ CpfUninterp s ]
      | Parsing.Parse_error -> (E.warn "Could not parse annotation %s\n" s); CpfCSAnn [ CpfUninterp s ]
      | Cpflex.CpfEndInput -> (E.warn "Could not parse annotation %s\n" s); CpfCSAnn [ CpfUninterp s ]
  in ast

