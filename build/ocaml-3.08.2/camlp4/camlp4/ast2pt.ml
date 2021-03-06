(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ast2pt.ml,v 1.31 2004/05/25 11:38:31 mauny Exp $ *)

open Stdpp;
open MLast;
open Parsetree;
open Longident;
open Asttypes;

value fast = ref False;
value no_constructors_arity = Pcaml.no_constructors_arity;

value get_tag x =
  if Obj.is_block (Obj.repr x) then Obj.tag (Obj.repr x) else Obj.magic x
;

value error loc str = raise_with_loc loc (Failure str);

value char_of_char_token loc s =
  try Token.eval_char s with [ Failure _ as exn -> raise_with_loc loc exn ]
;

value string_of_string_token loc s =
  try Token.eval_string loc s
  with [ Failure _ as exn -> raise_with_loc loc exn ]
;

value glob_fname = ref "";

value mkloc (bp, ep) =
  let loc_at n = 
    { (n) with
        Lexing.pos_fname = 
          if n.Lexing.pos_fname = "" then
            if glob_fname.val = "" then
              Pcaml.input_file.val
            else
              glob_fname.val
          else
            n.Lexing.pos_fname
      }
  in
  {Location.loc_start = loc_at bp;
   Location.loc_end = loc_at ep;
   Location.loc_ghost =
     bp.Lexing.pos_cnum = 0 && ep.Lexing.pos_cnum = 0}
;

value mkghloc (bp, ep) =
  let loc_at n = 
    { (n) with
        Lexing.pos_fname = 
          if n.Lexing.pos_fname = "" then
            if glob_fname.val = "" then
              Pcaml.input_file.val
            else
              glob_fname.val
          else
            n.Lexing.pos_fname
      }
  in
  {Location.loc_start = loc_at bp;
   Location.loc_end = loc_at ep;
   Location.loc_ghost = True}
;

value mktyp loc d = {ptyp_desc = d; ptyp_loc = mkloc loc};
value mkpat loc d = {ppat_desc = d; ppat_loc = mkloc loc};
value mkghpat loc d = {ppat_desc = d; ppat_loc = mkghloc loc};
value mkexp loc d = {pexp_desc = d; pexp_loc = mkloc loc};
value mkmty loc d = {pmty_desc = d; pmty_loc = mkloc loc};
value mksig loc d = {psig_desc = d; psig_loc = mkloc loc};
value mkmod loc d = {pmod_desc = d; pmod_loc = mkloc loc};
value mkstr loc d = {pstr_desc = d; pstr_loc = mkloc loc};
value mkfield loc d = {pfield_desc = d; pfield_loc = mkloc loc};
value mkcty loc d = {pcty_desc = d; pcty_loc = mkloc loc};
value mkpcl loc d = {pcl_desc = d; pcl_loc = mkloc loc};
value mkpolytype t =
  match t with
  [ TyPol _ _ _ -> t
  | _ -> TyPol (MLast.loc_of_ctyp t) [] t ]
;

value lident s = Lident s;
value ldot l s = Ldot l s;

value conv_con =
  let t = Hashtbl.create 73 in
  do {
    List.iter (fun (s, s') -> Hashtbl.add t s s')
      [("True", "true"); ("False", "false"); (" True", "True");
       (" False", "False")];
    fun s -> try Hashtbl.find t s with [ Not_found -> s ]
  }
;

value conv_lab =
  let t = Hashtbl.create 73 in
  do {
    List.iter (fun (s, s') -> Hashtbl.add t s s') [("val", "contents")];
    fun s -> try Hashtbl.find t s with [ Not_found -> s ]
  }
;

value array_function str name =
  ldot (lident str) (if fast.val then "unsafe_" ^ name else name)
;

value mkrf =
  fun
  [ True -> Recursive
  | False -> Nonrecursive ]
;

value mkli s =
  loop (fun s -> lident s) where rec loop f =
    fun
    [ [i :: il] -> loop (fun s -> ldot (f i) s) il
    | [] -> f s ]
;

value long_id_of_string_list loc sl =
  match List.rev sl with
  [ [] -> error loc "bad ast"
  | [s :: sl] -> mkli s (List.rev sl) ]
;

value rec ctyp_fa al =
  fun
  [ TyApp _ f a -> ctyp_fa [a :: al] f
  | f -> (f, al) ]
;

value rec ctyp_long_id_prefix t =
  match t with
  [ TyAcc _ m (TyLid _ s) ->
      error (loc_of_ctyp t) "invalid module expression"
  | TyAcc _ m (TyUid _ s) ->
      let (is_cls, li) = ctyp_long_id_prefix m in
      (is_cls, ldot li s)
  | TyApp _ m1 m2 ->
      let (is_cls, li1) = ctyp_long_id_prefix m1 in
      let (_, li2) = ctyp_long_id_prefix m2 in
      (is_cls, Lapply li1 li2)
  | TyUid _ s -> (False, lident s)
  | TyLid _ s ->
      error (loc_of_ctyp t) "invalid module expression"
  | t -> error (loc_of_ctyp t) "invalid module expression" ]
;

value ctyp_long_id t =
  match t with
  [ TyAcc _ m (TyLid _ s) ->
      let (is_cls, li) = ctyp_long_id_prefix m in
      (is_cls, ldot li s)
  | TyAcc _ m (TyUid _ s as t) ->
      error (loc_of_ctyp t) "invalid type name"
  | TyApp _ m1 m2 ->
      error (loc_of_ctyp t) "invalid type name"
  | TyUid _ s ->
      error (loc_of_ctyp t) "invalid type name"
  | TyLid _ s -> (False, lident s)
  | TyCls loc sl -> (True, long_id_of_string_list loc sl)
  | t -> error (loc_of_ctyp t) "invalid type" ]
;

value rec ctyp =
  fun
  [ TyAcc loc _ _ as f ->
      let (is_cls, li) = ctyp_long_id f in
      if is_cls then mktyp loc (Ptyp_class li [] [])
      else mktyp loc (Ptyp_constr li [])
  | TyAli loc t1 t2 ->
      let (t, i) =
        match (t1, t2) with
        [ (t, TyQuo _ s) -> (t, s)
        | (TyQuo _ s, t) -> (t, s)
        | _ -> error loc "invalid alias type" ]
      in
      mktyp loc (Ptyp_alias (ctyp t) i)
  | TyAny loc -> mktyp loc Ptyp_any
  | TyApp loc _ _ as f ->
      let (f, al) = ctyp_fa [] f in
      let (is_cls, li) = ctyp_long_id f in
      if is_cls then mktyp loc (Ptyp_class li (List.map ctyp al) [])
      else mktyp loc (Ptyp_constr li (List.map ctyp al))
  | TyArr loc (TyLab loc1 lab t1) t2 ->
      mktyp loc (Ptyp_arrow lab (ctyp t1) (ctyp t2))
  | TyArr loc (TyOlb loc1 lab t1) t2 ->
      let t1 = TyApp loc1 (TyLid loc1 "option") t1 in
      mktyp loc (Ptyp_arrow ("?" ^ lab) (ctyp t1) (ctyp t2))
  | TyArr loc t1 t2 -> mktyp loc (Ptyp_arrow "" (ctyp t1) (ctyp t2))
  | TyObj loc fl v -> mktyp loc (Ptyp_object (meth_list loc fl v))
  | TyCls loc id ->
      mktyp loc (Ptyp_class (long_id_of_string_list loc id) [] [])
  | TyLab loc _ _ -> error loc "labelled type not allowed here"
  | TyLid loc s -> mktyp loc (Ptyp_constr (lident s) [])
  | TyMan loc _ _ -> error loc "manifest type not allowed here"
  | TyOlb loc lab _ -> error loc "labelled type not allowed here"
  | TyPol loc pl t -> mktyp loc (Ptyp_poly pl (ctyp t))
  | TyQuo loc s -> mktyp loc (Ptyp_var s)
  | TyRec loc _ _ -> error loc "record type not allowed here"
  | TySum loc _ _ -> error loc "sum type not allowed here"
  | TyTup loc tl -> mktyp loc (Ptyp_tuple (List.map ctyp tl))
  | TyUid loc s as t -> error (loc_of_ctyp t) "invalid type"
  | TyVrn loc catl ool ->
      let catl =
        List.map
          (fun
           [ RfTag c a t -> Rtag c a (List.map ctyp t)
           | RfInh t -> Rinherit (ctyp t) ])
          catl
      in
      let (clos, sl) =
        match ool with
        [ None -> (True, None)
        | Some None -> (False, None)
        | Some (Some sl) -> (True, Some sl) ]
      in
      mktyp loc (Ptyp_variant catl clos sl) ]
and meth_list loc fl v =
  match fl with
  [ [] -> if v then [mkfield loc Pfield_var] else []
  | [(lab, t) :: fl] ->
      [mkfield loc (Pfield lab (ctyp (mkpolytype t))) :: meth_list loc fl v] ]
;

value mktype loc tl cl tk tm =
  let (params, variance) = List.split tl in
  {ptype_params = params; ptype_cstrs = cl; ptype_kind = tk;
   ptype_manifest = tm; ptype_loc = mkloc loc; ptype_variance = variance}
;
value mkmutable m = if m then Mutable else Immutable;
value mkprivate m = if m then Private else Public;
value mktrecord (_, n, m, t) = (n, mkmutable m, ctyp (mkpolytype t));
value mkvariant (_, c, tl) = (c, List.map ctyp tl);
value type_decl tl cl =
  fun
  [ TyMan loc t (TyRec _ pflag ltl) ->
      mktype loc tl cl (Ptype_record (List.map mktrecord ltl) (mkprivate pflag))
        (Some (ctyp t))
  | TyMan loc t (TySum _ pflag ctl) ->
      mktype loc tl cl (Ptype_variant (List.map mkvariant ctl) (mkprivate pflag))
        (Some (ctyp t))
  | TyRec loc pflag ltl ->
      mktype loc tl cl (Ptype_record (List.map mktrecord ltl) (mkprivate pflag)) None
  | TySum loc pflag ctl ->
      mktype loc tl cl (Ptype_variant (List.map mkvariant ctl) (mkprivate pflag)) None
  | t ->
      let m =
        match t with
        [ TyQuo _ s -> if List.mem_assoc s tl then Some (ctyp t) else None
        | _ -> Some (ctyp t) ]
      in
      mktype (loc_of_ctyp t) tl cl Ptype_abstract m ]
;

value mkvalue_desc t p = {pval_type = ctyp t; pval_prim = p};

value option f =
  fun
  [ Some x -> Some (f x)
  | None -> None ]
;

value expr_of_lab loc lab =
  fun
  [ Some e -> e
  | None -> ExLid loc lab ]
;

value patt_of_lab loc lab =
  fun
  [ Some p -> p
  | None -> PaLid loc lab ]
;

value paolab loc lab peoo =
  let lab =
    match (lab, peoo) with
    [ ("", Some (PaLid _ i | PaTyc _ (PaLid _ i) _, _)) -> i
    | ("", _) -> error loc "bad ast"
    | _ -> lab ]
  in
  let (p, eo) =
    match peoo with
    [ Some peo -> peo
    | None -> (PaLid loc lab, None) ]
  in
  (lab, p, eo)
;

value rec same_type_expr ct ce =
  match (ct, ce) with
  [ (TyLid _ s1, ExLid _ s2) -> s1 = s2
  | (TyUid _ s1, ExUid _ s2) -> s1 = s2
  | (TyAcc _ t1 t2, ExAcc _ e1 e2) ->
      same_type_expr t1 e1 && same_type_expr t2 e2
  | _ -> False ]
;

value rec common_id loc t e =
  match (t, e) with
  [ (TyLid _ s1, ExLid _ s2) when s1 = s2 -> lident s1
  | (TyUid _ s1, ExUid _ s2) when s1 = s2 -> lident s1
  | (TyAcc _ t1 (TyLid _ s1), ExAcc _ e1 (ExLid _ s2)) when s1 = s2 ->
      ldot (common_id loc t1 e1) s1
  | (TyAcc _ t1 (TyUid _ s1), ExAcc _ e1 (ExUid _ s2)) when s1 = s2 ->
      ldot (common_id loc t1 e1) s1
  | _ -> error loc "this expression should repeat the class id inherited" ]
;

value rec type_id loc t =
  match t with
  [ TyLid _ s1 -> lident s1
  | TyUid _ s1 -> lident s1
  | TyAcc _ t1 (TyLid _ s1) -> ldot (type_id loc t1) s1
  | TyAcc _ t1 (TyUid _ s1) -> ldot (type_id loc t1) s1
  | _ -> error loc "type identifier expected" ]
;

value rec module_type_long_id =
  fun
  [ MtAcc _ m (MtUid _ s) -> ldot (module_type_long_id m) s
  | MtAcc _ m (MtLid _ s) -> ldot (module_type_long_id m) s
  | MtApp _ m1 m2 -> Lapply (module_type_long_id m1) (module_type_long_id m2)
  | MtLid _ s -> lident s
  | MtUid _ s -> lident s
  | t -> error (loc_of_module_type t) "bad module type long ident" ]
;

value rec module_expr_long_id =
  fun
  [ MeAcc _ m (MeUid _ s) -> ldot (module_expr_long_id m) s
  | MeUid _ s -> lident s
  | t -> error (loc_of_module_expr t) "bad module expr long ident" ]
;

value mkwithc =
  fun
  [ WcTyp loc id tpl ct ->
      let (params, variance) = List.split tpl in
      (long_id_of_string_list loc id,
       Pwith_type
         {ptype_params = params; ptype_cstrs = [];
          ptype_kind = Ptype_abstract; ptype_manifest = Some (ctyp ct);
          ptype_loc = mkloc loc; ptype_variance = variance})
  | WcMod loc id m ->
      (long_id_of_string_list loc id, Pwith_module (module_expr_long_id m)) ]
;

value rec patt_fa al =
  fun
  [ PaApp _ f a -> patt_fa [a :: al] f
  | f -> (f, al) ]
;

value rec deep_mkrangepat loc c1 c2 =
  if c1 = c2 then mkghpat loc (Ppat_constant (Const_char c1))
  else
    mkghpat loc
      (Ppat_or (mkghpat loc (Ppat_constant (Const_char c1)))
         (deep_mkrangepat loc (Char.chr (Char.code c1 + 1)) c2))
;

value rec mkrangepat loc c1 c2 =
  if c1 > c2 then mkrangepat loc c2 c1
  else if c1 = c2 then mkpat loc (Ppat_constant (Const_char c1))
  else
    mkpat loc
      (Ppat_or (mkghpat loc (Ppat_constant (Const_char c1)))
         (deep_mkrangepat loc (Char.chr (Char.code c1 + 1)) c2))
;

value rec patt_long_id il =
  fun
  [ PaAcc _ p (PaUid _ i) -> patt_long_id [i :: il] p
  | p -> (p, il) ]
;

value rec patt_label_long_id =
  fun
  [ PaAcc _ m (PaLid _ s) -> ldot (patt_label_long_id m) (conv_lab s)
  | PaAcc _ m (PaUid _ s) -> ldot (patt_label_long_id m) s
  | PaUid _ s -> lident s
  | PaLid _ s -> lident (conv_lab s)
  | p -> error (loc_of_patt p) "bad label" ]
;

value rec patt =
  fun
  [ PaAcc loc p1 p2 ->
      let p =
        match patt_long_id [] p1 with
        [ (PaUid _ i, il) ->
            match p2 with
            [ PaUid _ s ->
                Ppat_construct (mkli (conv_con s) [i :: il]) None
                  (not no_constructors_arity.val)
            | _ -> error (loc_of_patt p2) "uppercase identifier expected" ]
        | _ -> error (loc_of_patt p2) "bad pattern" ]
      in
      mkpat loc p
  | PaAli loc p1 p2 ->
      let (p, i) =
        match (p1, p2) with
        [ (p, PaLid _ s) -> (p, s)
        | (PaLid _ s, p) -> (p, s)
        | _ -> error loc "invalid alias pattern" ]
      in
      mkpat loc (Ppat_alias (patt p) i)
  | PaAnt _ p -> patt p
  | PaAny loc -> mkpat loc Ppat_any
  | PaApp loc _ _ as f ->
      let (f, al) = patt_fa [] f in
      let al = List.map patt al in
      match (patt f).ppat_desc with
      [ Ppat_construct li None _ ->
          if no_constructors_arity.val then
            let a =
              match al with
              [ [a] -> a
              | _ -> mkpat loc (Ppat_tuple al) ]
            in
            mkpat loc (Ppat_construct li (Some a) False)
          else
            let a = mkpat loc (Ppat_tuple al) in
            mkpat loc (Ppat_construct li (Some a) True)
      | Ppat_variant s None ->
          let a =
            match al with
            [ [a] -> a
            | _ -> mkpat loc (Ppat_tuple al) ]
          in
          mkpat loc (Ppat_variant s (Some a))
      | _ ->
          error (loc_of_patt f)
            "this is not a constructor, it cannot be applied in a pattern" ]
  | PaArr loc pl -> mkpat loc (Ppat_array (List.map patt pl))
  | PaChr loc s ->
      mkpat loc (Ppat_constant (Const_char (char_of_char_token loc s)))
  | PaInt loc s -> mkpat loc (Ppat_constant (Const_int (int_of_string s)))
  | PaInt32 loc s -> mkpat loc (Ppat_constant (Const_int32 (Int32.of_string s)))
  | PaInt64 loc s -> mkpat loc (Ppat_constant (Const_int64 (Int64.of_string s)))
  | PaNativeInt loc s -> mkpat loc (Ppat_constant (Const_nativeint (Nativeint.of_string s)))
  | PaFlo loc s -> mkpat loc (Ppat_constant (Const_float s))
  | PaLab loc _ _ -> error loc "labeled pattern not allowed here"
  | PaLid loc s -> mkpat loc (Ppat_var s)
  | PaOlb loc _ _ -> error loc "labeled pattern not allowed here"
  | PaOrp loc p1 p2 -> mkpat loc (Ppat_or (patt p1) (patt p2))
  | PaRng loc p1 p2 ->
      match (p1, p2) with
      [ (PaChr loc1 c1, PaChr loc2 c2) ->
          let c1 = char_of_char_token loc1 c1 in
          let c2 = char_of_char_token loc2 c2 in
          mkrangepat loc c1 c2
      | _ -> error loc "range pattern allowed only for characters" ]
  | PaRec loc lpl -> mkpat loc (Ppat_record (List.map mklabpat lpl))
  | PaStr loc s ->
      mkpat loc (Ppat_constant (Const_string (string_of_string_token loc s)))
  | PaTup loc pl -> mkpat loc (Ppat_tuple (List.map patt pl))
  | PaTyc loc p t -> mkpat loc (Ppat_constraint (patt p) (ctyp t))
  | PaTyp loc sl -> mkpat loc (Ppat_type (long_id_of_string_list loc sl))
  | PaUid loc s ->
      let ca = not no_constructors_arity.val in
      mkpat loc (Ppat_construct (lident (conv_con s)) None ca)
  | PaVrn loc s -> mkpat loc (Ppat_variant s None) ]
and mklabpat (lab, p) = (patt_label_long_id lab, patt p);

value rec expr_fa al =
  fun
  [ ExApp _ f a -> expr_fa [a :: al] f
  | f -> (f, al) ]
;

value rec class_expr_fa al =
  fun
  [ CeApp _ ce a -> class_expr_fa [a :: al] ce
  | ce -> (ce, al) ]
;

value rec sep_expr_acc l =
  fun
  [ ExAcc _ e1 e2 -> sep_expr_acc (sep_expr_acc l e2) e1
  | ExUid ((bp, _) as loc) s as e ->
      match l with
      [ [] -> [(loc, [], e)]
      | [((_, ep), sl, e) :: l] -> [((bp, ep), [s :: sl], e) :: l] ]
  | e -> [(loc_of_expr e, [], e) :: l] ]
;

(*
value expr_label_long_id e =
  match sep_expr_acc [] e with
  [ [(_, ml, ExLid _ s)] -> mkli (conv_lab s) ml
  | _ -> error (loc_of_expr e) "invalid label" ]
;
*)

value class_info class_expr ci =
  let (params, variance) = List.split (snd ci.ciPrm) in
  {pci_virt = if ci.ciVir then Virtual else Concrete;
   pci_params = (params, mkloc (fst ci.ciPrm)); pci_name = ci.ciNam;
   pci_expr = class_expr ci.ciExp; pci_loc = mkloc ci.ciLoc;
   pci_variance = variance}
;

value apply_with_var v x f =
  let vx = v.val in
  try
    do {
      v.val := x;
      let r = f ();
      v.val := vx;
      r
    }
  with e -> do { v.val := vx; raise e }
;

value rec expr =
  fun
  [ ExAcc loc x (ExLid _ "val") ->
      mkexp loc
        (Pexp_apply (mkexp loc (Pexp_ident (Lident "!"))) [("", expr x)])
  | ExAcc loc _ _ as e ->
      let (e, l) =
        match sep_expr_acc [] e with
        [ [(loc, ml, ExUid _ s) :: l] ->
            let ca = not no_constructors_arity.val in
            (mkexp loc (Pexp_construct (mkli s ml) None ca), l)
        | [(loc, ml, ExLid _ s) :: l] ->
            (mkexp loc (Pexp_ident (mkli s ml)), l)
        | [(_, [], e) :: l] -> (expr e, l)
        | _ -> error loc "bad ast" ]
      in
      let (_, e) =
        List.fold_left
          (fun ((bp, _), e1) ((_, ep), ml, e2) ->
             match e2 with
             [ ExLid _ s ->
                 let loc = (bp, ep) in
                 (loc, mkexp loc (Pexp_field e1 (mkli (conv_lab s) ml)))
             | _ -> error (loc_of_expr e2) "lowercase identifier expected" ])
          (loc, e) l
      in
      e
  | ExAnt _ e -> expr e
  | ExApp loc _ _ as f ->
      let (f, al) = expr_fa [] f in
      let al = List.map label_expr al in
      match (expr f).pexp_desc with
      [ Pexp_construct li None _ ->
          let al = List.map snd al in
          if no_constructors_arity.val then
            let a =
              match al with
              [ [a] -> a
              | _ -> mkexp loc (Pexp_tuple al) ]
            in
            mkexp loc (Pexp_construct li (Some a) False)
          else
            let a = mkexp loc (Pexp_tuple al) in
            mkexp loc (Pexp_construct li (Some a) True)
      | Pexp_variant s None ->
          let al = List.map snd al in
          let a =
            match al with
            [ [a] -> a
            | _ -> mkexp loc (Pexp_tuple al) ]
          in
          mkexp loc (Pexp_variant s (Some a))
      | _ -> mkexp loc (Pexp_apply (expr f) al) ]
  | ExAre loc e1 e2 ->
      mkexp loc
        (Pexp_apply (mkexp loc (Pexp_ident (array_function "Array" "get")))
           [("", expr e1); ("", expr e2)])
  | ExArr loc el -> mkexp loc (Pexp_array (List.map expr el))
  | ExAsf loc -> mkexp loc Pexp_assertfalse
  | ExAss loc e v ->
      let e =
        match e with
        [ ExAcc loc x (ExLid _ "val") ->
            Pexp_apply (mkexp loc (Pexp_ident (Lident ":=")))
              [("", expr x); ("", expr v)]
        | ExAcc loc _ _ ->
            match (expr e).pexp_desc with
            [ Pexp_field e lab -> Pexp_setfield e lab (expr v)
            | _ -> error loc "bad record access" ]
        | ExAre _ e1 e2 ->
            Pexp_apply (mkexp loc (Pexp_ident (array_function "Array" "set")))
              [("", expr e1); ("", expr e2); ("", expr v)]
        | ExLid _ lab -> Pexp_setinstvar lab (expr v)
        | ExSte _ e1 e2 ->
            Pexp_apply
              (mkexp loc (Pexp_ident (array_function "String" "set")))
              [("", expr e1); ("", expr e2); ("", expr v)]
        | _ -> error loc "bad left part of assignment" ]
      in
      mkexp loc e
  | ExAsr loc e -> mkexp loc (Pexp_assert (expr e))
  | ExChr loc s ->
      mkexp loc (Pexp_constant (Const_char (char_of_char_token loc s)))
  | ExCoe loc e t1 t2 ->
      mkexp loc (Pexp_constraint (expr e) (option ctyp t1) (Some (ctyp t2)))
  | ExFlo loc s -> mkexp loc (Pexp_constant (Const_float s))
  | ExFor loc i e1 e2 df el ->
      let e3 = ExSeq loc el in
      let df = if df then Upto else Downto in
      mkexp loc (Pexp_for i (expr e1) (expr e2) df (expr e3))
  | ExFun loc [(PaLab _ lab po, w, e)] ->
      mkexp loc
        (Pexp_function lab None
           [(patt (patt_of_lab loc lab po), when_expr e w)])
  | ExFun loc [(PaOlb _ lab peoo, w, e)] ->
      let (lab, p, eo) = paolab loc lab peoo in
      mkexp loc
        (Pexp_function ("?" ^ lab) (option expr eo) [(patt p, when_expr e w)])
  | ExFun loc pel -> mkexp loc (Pexp_function "" None (List.map mkpwe pel))
  | ExIfe loc e1 e2 e3 ->
      mkexp loc (Pexp_ifthenelse (expr e1) (expr e2) (Some (expr e3)))
  | ExInt loc s -> mkexp loc (Pexp_constant (Const_int (int_of_string s)))
  | ExInt32 loc s -> mkexp loc (Pexp_constant (Const_int32 (Int32.of_string s)))
  | ExInt64 loc s -> mkexp loc (Pexp_constant (Const_int64 (Int64.of_string s)))
  | ExNativeInt loc s -> mkexp loc (Pexp_constant (Const_nativeint (Nativeint.of_string s)))
  | ExLab loc _ _ -> error loc "labeled expression not allowed here"
  | ExLaz loc e -> mkexp loc (Pexp_lazy (expr e))
  | ExLet loc rf pel e ->
      mkexp loc (Pexp_let (mkrf rf) (List.map mkpe pel) (expr e))
  | ExLid loc s -> mkexp loc (Pexp_ident (lident s))
  | ExLmd loc i me e -> mkexp loc (Pexp_letmodule i (module_expr me) (expr e))
  | ExMat loc e pel -> mkexp loc (Pexp_match (expr e) (List.map mkpwe pel))
  | ExNew loc id -> mkexp loc (Pexp_new (long_id_of_string_list loc id))
  | ExObj loc po cfl -> 
      let p =
        match po with
        [ Some p -> p
        | None -> PaAny loc ]
      in
      let cil = List.fold_right class_str_item cfl [] in
      mkexp loc (Pexp_object (patt p, cil))
  | ExOlb loc _ _ -> error loc "labeled expression not allowed here"
  | ExOvr loc iel -> mkexp loc (Pexp_override (List.map mkideexp iel))
  | ExRec loc lel eo ->
      if lel = [] then error loc "empty record"
      else
        let eo =
          match eo with
          [ Some e -> Some (expr e)
          | None -> None ]
        in
        mkexp loc (Pexp_record (List.map mklabexp lel) eo)
  | ExSeq loc el ->
      let rec loop =
        fun
        [ [] -> expr (ExUid loc "()")
        | [e] -> expr e
        | [e :: el] ->
            let loc = (fst (loc_of_expr e), snd loc) in
            mkexp loc (Pexp_sequence (expr e) (loop el)) ]
      in
      loop el
  | ExSnd loc e s -> mkexp loc (Pexp_send (expr e) s)
  | ExSte loc e1 e2 ->
      mkexp loc
        (Pexp_apply (mkexp loc (Pexp_ident (array_function "String" "get")))
           [("", expr e1); ("", expr e2)])
  | ExStr loc s ->
      mkexp loc (Pexp_constant (Const_string (string_of_string_token loc s)))
  | ExTry loc e pel -> mkexp loc (Pexp_try (expr e) (List.map mkpwe pel))
  | ExTup loc el -> mkexp loc (Pexp_tuple (List.map expr el))
  | ExTyc loc e t -> mkexp loc (Pexp_constraint (expr e) (Some (ctyp t)) None)
  | ExUid loc s ->
      let ca = not no_constructors_arity.val in
      mkexp loc (Pexp_construct (lident (conv_con s)) None ca)
  | ExVrn loc s -> mkexp loc (Pexp_variant s None)
  | ExWhi loc e1 el ->
      let e2 = ExSeq loc el in
      mkexp loc (Pexp_while (expr e1) (expr e2)) ]
and label_expr =
  fun
  [ ExLab loc lab eo -> (lab, expr (expr_of_lab loc lab eo))
  | ExOlb loc lab eo -> ("?" ^ lab, expr (expr_of_lab loc lab eo))
  | e -> ("", expr e) ]
and mkpe (p, e) = (patt p, expr e)
and mkpwe (p, w, e) = (patt p, when_expr e w)
and when_expr e =
  fun
  [ Some w -> mkexp (loc_of_expr e) (Pexp_when (expr w) (expr e))
  | None -> expr e ]
and mklabexp (lab, e) = (patt_label_long_id lab, expr e)
and mkideexp (ide, e) = (ide, expr e)
and mktype_decl ((loc, c), tl, td, cl) =
  let cl =
    List.map
      (fun (t1, t2) ->
         let loc = (fst (loc_of_ctyp t1), snd (loc_of_ctyp t2)) in
         (ctyp t1, ctyp t2, mkloc loc))
      cl
  in
  (c, type_decl tl cl td)
and module_type =
  fun
  [ MtAcc loc _ _ as f -> mkmty loc (Pmty_ident (module_type_long_id f))
  | MtApp loc _ _ as f -> mkmty loc (Pmty_ident (module_type_long_id f))
  | MtFun loc n nt mt ->
      mkmty loc (Pmty_functor n (module_type nt) (module_type mt))
  | MtLid loc s -> mkmty loc (Pmty_ident (lident s))
  | MtQuo loc _ -> error loc "abstract module type not allowed here"
  | MtSig loc sl ->
      mkmty loc (Pmty_signature (List.fold_right sig_item sl []))
  | MtUid loc s -> mkmty loc (Pmty_ident (lident s))
  | MtWit loc mt wcl ->
      mkmty loc (Pmty_with (module_type mt) (List.map mkwithc wcl)) ]
and sig_item s l =
  match s with
  [ SgCls loc cd ->
      [mksig loc (Psig_class (List.map (class_info class_type) cd)) :: l]
  | SgClt loc ctd ->
      [mksig loc (Psig_class_type (List.map (class_info class_type) ctd)) ::
       l]
  | SgDcl loc sl -> List.fold_right sig_item sl l
  | SgDir loc _ _ -> l
  | SgExc loc n tl -> [mksig loc (Psig_exception n (List.map ctyp tl)) :: l]
  | SgExt loc n t p -> [mksig loc (Psig_value n (mkvalue_desc t p)) :: l]
  | SgInc loc mt -> [mksig loc (Psig_include (module_type mt)) :: l]
  | SgMod loc n mt -> [mksig loc (Psig_module n (module_type mt)) :: l]
  | SgRecMod loc nmts ->
      [mksig loc (Psig_recmodule (List.map (fun (n,mt) -> (n, module_type mt)) nmts)) :: l]
  | SgMty loc n mt ->
      let si =
        match mt with
        [ MtQuo _ _ -> Pmodtype_abstract
        | _ -> Pmodtype_manifest (module_type mt) ]
      in
      [mksig loc (Psig_modtype n si) :: l]
  | SgOpn loc id ->
      [mksig loc (Psig_open (long_id_of_string_list loc id)) :: l]
  | SgTyp loc tdl -> [mksig loc (Psig_type (List.map mktype_decl tdl)) :: l]
  | SgUse loc fn sl ->
      apply_with_var glob_fname fn
        (fun () -> List.fold_right (fun (si, _) -> sig_item si) sl l)
  | SgVal loc n t -> [mksig loc (Psig_value n (mkvalue_desc t [])) :: l] ]
and module_expr =
  fun
  [ MeAcc loc _ _ as f -> mkmod loc (Pmod_ident (module_expr_long_id f))
  | MeApp loc me1 me2 ->
      mkmod loc (Pmod_apply (module_expr me1) (module_expr me2))
  | MeFun loc n mt me ->
      mkmod loc (Pmod_functor n (module_type mt) (module_expr me))
  | MeStr loc sl ->
      mkmod loc (Pmod_structure (List.fold_right str_item sl []))
  | MeTyc loc me mt ->
      mkmod loc (Pmod_constraint (module_expr me) (module_type mt))
  | MeUid loc s -> mkmod loc (Pmod_ident (lident s)) ]
and str_item s l =
  match s with
  [ StCls loc cd ->
      [mkstr loc (Pstr_class (List.map (class_info class_expr) cd)) :: l]
  | StClt loc ctd ->
      [mkstr loc (Pstr_class_type (List.map (class_info class_type) ctd)) ::
       l]
  | StDcl loc sl -> List.fold_right str_item sl l
  | StDir loc _ _ -> l
  | StExc loc n tl sl ->
      let si =
        match (tl, sl) with
        [ (tl, []) -> Pstr_exception n (List.map ctyp tl)
        | ([], sl) -> Pstr_exn_rebind n (long_id_of_string_list loc sl)
        | _ -> error loc "bad exception declaration" ]
      in
      [mkstr loc si :: l]
  | StExp loc e -> [mkstr loc (Pstr_eval (expr e)) :: l]
  | StExt loc n t p -> [mkstr loc (Pstr_primitive n (mkvalue_desc t p)) :: l]
  | StInc loc me -> [mkstr loc (Pstr_include (module_expr me)) :: l]
  | StMod loc n me -> [mkstr loc (Pstr_module n (module_expr me)) :: l]
  | StRecMod loc nmes ->
      [mkstr loc
         (Pstr_recmodule
            (List.map
               (fun (n,mt,me) -> (n, module_type mt, module_expr me))
               nmes)) :: l]
  | StMty loc n mt -> [mkstr loc (Pstr_modtype n (module_type mt)) :: l]
  | StOpn loc id ->
      [mkstr loc (Pstr_open (long_id_of_string_list loc id)) :: l]
  | StTyp loc tdl -> [mkstr loc (Pstr_type (List.map mktype_decl tdl)) :: l]
  | StUse loc fn sl ->
      apply_with_var glob_fname fn
        (fun () -> List.fold_right (fun (si, _) -> str_item si) sl l)
  | StVal loc rf pel ->
      [mkstr loc (Pstr_value (mkrf rf) (List.map mkpe pel)) :: l] ]
and class_type =
  fun
  [ CtCon loc id tl ->
      mkcty loc
        (Pcty_constr (long_id_of_string_list loc id) (List.map ctyp tl))
  | CtFun loc (TyLab _ lab t) ct ->
      mkcty loc (Pcty_fun lab (ctyp t) (class_type ct))
  | CtFun loc (TyOlb loc1 lab t) ct ->
      let t = TyApp loc1 (TyLid loc1 "option") t in
      mkcty loc (Pcty_fun ("?" ^ lab) (ctyp t) (class_type ct))
  | CtFun loc t ct -> mkcty loc (Pcty_fun "" (ctyp t) (class_type ct))
  | CtSig loc t_o ctfl ->
      let t =
        match t_o with
        [ Some t -> t
        | None -> TyAny loc ]
      in
      let cil = List.fold_right class_sig_item ctfl [] in
      mkcty loc (Pcty_signature (ctyp t, cil)) ]
and class_sig_item c l =
  match c with
  [ CgCtr loc t1 t2 -> [Pctf_cstr (ctyp t1, ctyp t2, mkloc loc) :: l]
  | CgDcl loc cl -> List.fold_right class_sig_item cl l
  | CgInh loc ct -> [Pctf_inher (class_type ct) :: l]
  | CgMth loc s pf t ->
      [Pctf_meth (s, mkprivate pf, ctyp (mkpolytype t), mkloc loc) :: l]
  | CgVal loc s b t ->
      [Pctf_val (s, mkmutable b, Some (ctyp t), mkloc loc) :: l]
  | CgVir loc s b t ->
      [Pctf_virt (s, mkprivate b, ctyp (mkpolytype t), mkloc loc) :: l] ]
and class_expr =
  fun
  [ CeApp loc _ _ as c ->
      let (ce, el) = class_expr_fa [] c in
      let el = List.map label_expr el in
      mkpcl loc (Pcl_apply (class_expr ce) el)
  | CeCon loc id tl ->
      mkpcl loc
        (Pcl_constr (long_id_of_string_list loc id) (List.map ctyp tl))
  | CeFun loc (PaLab _ lab po) ce ->
      mkpcl loc
        (Pcl_fun lab None (patt (patt_of_lab loc lab po)) (class_expr ce))
  | CeFun loc (PaOlb _ lab peoo) ce ->
      let (lab, p, eo) = paolab loc lab peoo in
      mkpcl loc
        (Pcl_fun ("?" ^ lab) (option expr eo) (patt p) (class_expr ce))
  | CeFun loc p ce -> mkpcl loc (Pcl_fun "" None (patt p) (class_expr ce))
  | CeLet loc rf pel ce ->
      mkpcl loc (Pcl_let (mkrf rf) (List.map mkpe pel) (class_expr ce))
  | CeStr loc po cfl ->
      let p =
        match po with
        [ Some p -> p
        | None -> PaAny loc ]
      in
      let cil = List.fold_right class_str_item cfl [] in
      mkpcl loc (Pcl_structure (patt p, cil))
  | CeTyc loc ce ct ->
      mkpcl loc (Pcl_constraint (class_expr ce) (class_type ct)) ]
and class_str_item c l =
  match c with
  [ CrCtr loc t1 t2 -> [Pcf_cstr (ctyp t1, ctyp t2, mkloc loc) :: l]
  | CrDcl loc cl -> List.fold_right class_str_item cl l
  | CrInh loc ce pb -> [Pcf_inher (class_expr ce) pb :: l]
  | CrIni loc e -> [Pcf_init (expr e) :: l]
  | CrMth loc s b e t ->
      let t = option (fun t -> ctyp (mkpolytype t)) t in
      let e = mkexp loc (Pexp_poly (expr e) t) in
      [Pcf_meth (s, mkprivate b, e, mkloc loc) :: l]
  | CrVal loc s b e -> [Pcf_val (s, mkmutable b, expr e, mkloc loc) :: l]
  | CrVir loc s b t ->
      [Pcf_virt (s, mkprivate b, ctyp (mkpolytype t), mkloc loc) :: l] ]
;

value interf ast = List.fold_right sig_item ast [];
value implem ast = List.fold_right str_item ast [];

value directive loc =
  fun
  [ None -> Pdir_none
  | Some (ExStr _ s) -> Pdir_string s
  | Some (ExInt _ i) -> Pdir_int (int_of_string i)
  | Some (ExUid _ "True") -> Pdir_bool True
  | Some (ExUid _ "False") -> Pdir_bool False
  | Some e ->
      let sl =
        loop e where rec loop =
          fun
          [ ExLid _ i | ExUid _ i -> [i]
          | ExAcc _ e (ExLid _ i) | ExAcc _ e (ExUid _ i) -> loop e @ [i]
          | e -> raise_with_loc (loc_of_expr e) (Failure "bad ast") ]
      in
      Pdir_ident (long_id_of_string_list loc sl) ]
;

value phrase =
  fun
  [ StDir loc d dp -> Ptop_dir d (directive loc dp)
  | si -> Ptop_def (str_item si []) ]
;
