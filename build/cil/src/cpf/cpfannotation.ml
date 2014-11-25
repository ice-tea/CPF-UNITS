(* TODO: May want to just keep uninterp versions. *)

(* Data type for CPF annotations *)
type cpf_annotation =
    CpfCSAnn of cpf_exp list
  | CpfELAnn of cpf_exp list
and cpf_exp =
    CpfDerefExp of cpf_exp
  | CpfParenExp of cpf_annotation
  | CpfDotOffset of cpf_exp * cpf_exp
  | CpfArrowOffset of cpf_exp * cpf_exp
  | CpfVarExp of string
  | CpfFSymExp of string
  | CpfVSymExp of string
  | CpfUninterp of string
  | CpfNum of string

(* Pretty-print an annotation *)
let rec show_cpf_annotation (ann:cpf_annotation) =
  match ann with
    | CpfCSAnn cs -> show_wsep "," cs
    | CpfELAnn el -> show_wsep " " el
and show_cpf_exp (e:cpf_exp) =
  match e with
    | CpfDerefExp e' -> "*" ^ (show_cpf_exp e')
    | CpfParenExp a -> "(" ^ (show_cpf_annotation a) ^ ")"
    | CpfDotOffset (el,er) -> (show_cpf_exp el) ^ "." ^ (show_cpf_exp er)
    | CpfArrowOffset (el,er) -> (show_cpf_exp el) ^ "->" ^ (show_cpf_exp er)
    | CpfVarExp s -> s
    | CpfFSymExp s -> "@" ^ s
    | CpfVSymExp s -> "$" ^ s
    | CpfUninterp s -> s 
    | CpfNum s -> s
and show_wsep (sep:string) (el:cpf_exp list) =
  match el with
    | e1::e2::el -> (show_cpf_exp e1) ^ sep ^ (show_wsep sep (e2::el))
    | e2::[] -> show_cpf_exp e2
    | [] -> ""

(* Pretty-print an annotation with extra spaces, useful for Maude *)
let rec show_cpf_annotation_wspaces (ann:cpf_annotation) =
  match ann with
    | CpfCSAnn cs -> show_wsep_wspaces ", " cs
    | CpfELAnn el -> show_wsep_wspaces " " el
and show_cpf_exp_wspaces (e:cpf_exp) =
  match e with
    | CpfDerefExp e' -> "* " ^ (show_cpf_exp_wspaces e')
    | CpfParenExp a -> "(" ^ (show_cpf_annotation_wspaces a) ^ ")"
    | CpfDotOffset (el,er) -> (show_cpf_exp_wspaces el) ^ " . " ^ (show_cpf_exp_wspaces er)
    | CpfArrowOffset (el,er) -> (show_cpf_exp_wspaces el) ^ " -> " ^ (show_cpf_exp_wspaces er)
    | CpfVarExp s -> s
    | CpfFSymExp s -> "@" ^ s
    | CpfVSymExp s -> "$" ^ s
    | CpfUninterp s -> s 
    | CpfNum s -> s
and show_wsep_wspaces (sep:string) (el:cpf_exp list) =
  match el with
    | e1::e2::el -> (show_cpf_exp_wspaces e1) ^ sep ^ (show_wsep_wspaces sep (e2::el))
    | e2::[] -> show_cpf_exp_wspaces e2
    | [] -> ""

(* Get a list of the variable names in an annotation. Note that this does
   NOT return names of structure fields, etc -- only non-offset names used
   to name variables (e.g., for a.b or x->y, b and y will not be returned. *)
let rec get_ann_name_list (ann:cpf_annotation) : (string list) =
  match ann with
    | CpfCSAnn cs -> List.fold_left (fun x y -> (get_exp_name_list y) @ x) [] cs
    | CpfELAnn el -> List.fold_left (fun x y -> (get_exp_name_list y) @ x) [] el 
and get_exp_name_list (e:cpf_exp) =
  match e with
    | CpfDerefExp e' -> get_exp_name_list e'
    | CpfParenExp a -> get_ann_name_list a
    | CpfDotOffset (el,er) -> (get_exp_name_list el) 
    | CpfArrowOffset (el,er) -> (get_exp_name_list el) 
    | CpfVarExp s -> [ s ]
    | CpfFSymExp _ -> [] 
    | CpfVSymExp _ -> []
    | CpfUninterp _ -> []
    | CpfNum _ -> []

(* Replace variable names. Again, note that this does NOT replace names through
   offsets, which would represent structure fields. *)
let rec replace_ann_name (ann:cpf_annotation) (n:string) (n':string) : (cpf_annotation) =
  match ann with
    | CpfCSAnn cs -> CpfCSAnn (List.map (replace_exp_name n n') cs)
    | CpfELAnn el -> CpfELAnn (List.map (replace_exp_name n n') el)
and replace_exp_name (n:string) (n':string) (e:cpf_exp) : (cpf_exp) =
  match e with
    | CpfDerefExp e' -> CpfDerefExp (replace_exp_name n n' e')
    | CpfParenExp a -> CpfParenExp (replace_ann_name a n n')
    | CpfDotOffset (el,er) -> CpfDotOffset ((replace_exp_name n n' el), er)
    | CpfArrowOffset (el,er) -> CpfArrowOffset ((replace_exp_name n n' el), er)
    | CpfVarExp s -> if (s = n) then (CpfVarExp n') else e
    | CpfFSymExp _ -> e
    | CpfVSymExp _ -> e
    | CpfUninterp _ -> e
    | CpfNum _ -> e

(* Similar to replace_ann_name, but replaces the var name with an uninterpreted block;
   this is useful if we want to do more processing over the AST and don't want to change
   the variable name again inadvertently. *)
let rec replace_ann_name_uninterp (ann:cpf_annotation) (n:string) (n':string) : (cpf_annotation) =
  match ann with
    | CpfCSAnn cs -> CpfCSAnn (List.map (replace_exp_name_uninterp n n') cs)
    | CpfELAnn el -> CpfELAnn (List.map (replace_exp_name_uninterp n n') el)
and replace_exp_name_uninterp (n:string) (n':string) (e:cpf_exp) : (cpf_exp) =
  match e with
    | CpfDerefExp e' -> CpfDerefExp (replace_exp_name_uninterp n n' e')
    | CpfParenExp a -> CpfParenExp (replace_ann_name_uninterp a n n')
    | CpfDotOffset (el,er) -> CpfDotOffset ((replace_exp_name_uninterp n n' el), er)
    | CpfArrowOffset (el,er) -> CpfArrowOffset ((replace_exp_name_uninterp n n' el), er)
    | CpfVarExp s -> if (s = n) then (CpfUninterp n') else e
    | CpfFSymExp _ -> e
    | CpfVSymExp _ -> e
    | CpfUninterp _ -> e
    | CpfNum _ -> e

(* The same as replace_ann_name_uninterp, but this replaces a function symbol, like
   @sym, with an uninterpreted block instead of replacing a variable. *)
let rec replace_ann_fsym_uninterp (ann:cpf_annotation) (n:string) (n':string) : (cpf_annotation) =
  match ann with
    | CpfCSAnn cs -> CpfCSAnn (List.map (replace_exp_fsym_uninterp n n') cs)
    | CpfELAnn el -> CpfELAnn (List.map (replace_exp_fsym_uninterp n n') el)
and replace_exp_fsym_uninterp (n:string) (n':string) (e:cpf_exp) : (cpf_exp) =
  match e with
    | CpfDerefExp e' -> CpfDerefExp (replace_exp_fsym_uninterp n n' e')
    | CpfParenExp a -> CpfParenExp (replace_ann_fsym_uninterp a n n')
    | CpfDotOffset (el,er) -> CpfDotOffset ((replace_exp_fsym_uninterp n n' el), er)
    | CpfArrowOffset (el,er) -> CpfArrowOffset ((replace_exp_fsym_uninterp n n' el), er)
    | CpfVarExp _ -> e
    | CpfFSymExp s -> if (s = n) then (CpfUninterp n') else e
    | CpfVSymExp _ -> e
    | CpfUninterp _ -> e
    | CpfNum _ -> e

(* Replace any variable names with a Maude variant, wrapping them in n('_), with
   the name in position _. This also works for offset names, so it will descend into
   individual structure fields and chains of fields, like a.b.c. *)
let rec maudeify_ann_names (ann:cpf_annotation) : (cpf_annotation) =
  match ann with
    | CpfCSAnn cs -> CpfCSAnn (List.map maudeify_exp_names cs)
    | CpfELAnn el -> CpfELAnn (List.map maudeify_exp_names el)
and maudeify_exp_names (e:cpf_exp) =
  match e with
    | CpfDerefExp e' -> CpfDerefExp (maudeify_exp_names e')
    | CpfParenExp a -> CpfParenExp (maudeify_ann_names a)
    | CpfDotOffset (el,er) -> CpfDotOffset(maudeify_exp_names el, maudeify_exp_names er)
    | CpfArrowOffset (el,er) -> CpfArrowOffset(maudeify_exp_names el, maudeify_exp_names er)
    | CpfVarExp s -> CpfUninterp ("n('" ^ s ^ ")")
    | CpfFSymExp _ -> e 
    | CpfVSymExp _ -> e
    | CpfUninterp _ -> e
    | CpfNum _ -> e

