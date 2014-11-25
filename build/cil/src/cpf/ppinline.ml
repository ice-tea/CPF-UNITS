(*
TODO

1. When making type variables unique, we need to make sure
   that, if temps were created to hold the result, the
   annotation on the temp is also changed (should be
   able to check this by looking at call to see if the
   result is being assigned anywhere, then modify the
   annotation on the assign as well)

2. We should policy filter assume, assert, and invariant
   instructions here as well, since we have all the info
   we need to do so, and are already manipulating all the
   annotation-related instructions
*)

open Pretty
open Cil
open Callgraph
open Funhash
open Findglobals
open Cpfutil
open Cpfannotation
module E = Errormsg
module H = Hashtbl
module F = Fslutil
module P = Ptranal

module StringCompare =
  struct
    type t = string
    let compare = compare
  end

module VSet = Set.Make(StringCompare)

module VMap = Map.Make(StringCompare)

(***********************************************************************)
(*                                                                     *)
(* Global Variables                                                    *)
(*                                                                     *)
(***********************************************************************)

let default_policy = ref "ALL"
let use_aa = ref false

(***********************************************************************)
(*                                                                     *)
(* Helper Functions                                                    *)
(*                                                                     *)
(***********************************************************************)

(* Given a prepost, get the string component *)
let get_pp_str (p:prepost) : string =
  match p with
    | PreCond (x,s) -> s
    | PostCond (x,s) -> s
    | Modifies (x,s) -> s
    | TInvariant (x,s) -> s

(* Given a prepost, get the policy tag component *)
let get_pp_tag (p:prepost) : string =
  match p with
    | PreCond (x,s) -> x
    | PostCond (x,s) -> x
    | Modifies (x,s) -> x
    | TInvariant (x,s) -> x

(* Replace the string component of a prepost *)
let replace_pp_str (p:prepost) (s:string) : prepost =
  match p with
    | PreCond (x,_) -> PreCond (x,s)
    | PostCond (x,_) -> PostCond (x,s)
    | Modifies (x,_) -> Modifies (x,s)
    | TInvariant (x,_) -> TInvariant (x,s)

(* Given a list of variables, rename each occurence of var's original name *)
(* in the prepost annotation with var's new name. This is needed because CIL alpha *)
(* converts var names, but annotations are given in the original name *)
let rename_pp_vars (p:prepost) (vl:varinfo list) : prepost =
  let s = get_pp_str p
  in let ast = ann_ast s
  in let ast' = List.fold_left (fun x y -> (replace_ann_name_uninterp x y.vorigname y.vname)) ast vl
  in let s' = show_cpf_annotation_wspaces ast'
  in replace_pp_str p s'

(* Maude-ify variable names so that they can be properly handled by Maude. This now maude-ifies *)
(* other names as well, so we can do it all in one shot. If not, we can have problems with unparsing *)
(* and parsing the same string multiple times, since we parse names in the middle of uninterpreted *)
(* blocks of text. *)
let maudeify_vars (p:prepost) (vl:varinfo list) : prepost =
  let s = get_pp_str p
  in let ast = ann_ast s
  in let ast' = List.fold_left (fun x y -> (replace_ann_name_uninterp x y.vname ((maudeNamePrefix y) ^ "('" ^ y.vname ^ ")"))) ast vl
  in let ast'' = maudeify_ann_names ast'
  in let s' = show_cpf_annotation_wspaces ast''
  in replace_pp_str p s'

(* The same as maudeify_vars, but also processes a series of replacements for fsyms. *)
let maudeify_vars_wrepl (p:prepost) (vl:varinfo list) (rl:(string*string)list) : prepost =
  let s = get_pp_str p
  in let ast = ann_ast s
  in let ast' = List.fold_left (fun x y -> (replace_ann_name_uninterp x y.vname ((maudeNamePrefix y) ^ "('" ^ y.vname ^ ")"))) ast vl
  in let ast'' = List.fold_left (fun x y -> let (f,t) = y in replace_ann_fsym_uninterp x f t) ast' rl
  in let ast''' = maudeify_ann_names ast''
  in let s' = show_cpf_annotation_wspaces ast'''
  in replace_pp_str p s'

(* The same as maudeify_vars_wrepl, but vars are now just strings, since we *)
(* may not have varinfo records. vl is a list of var name -> string replacements, *)
(* gl is a list of var name -> var name replacements, and rl is a list of *)
(* f symbol -> string replacements. *)
let maudeify_vars_exps_wrepl (p:prepost) (vl:(string*string)list) (gl:(string*string)list) (rl:(string*string)list) : prepost =
  let s = get_pp_str p
  in let ast = ann_ast s
  in let ast1 = List.fold_left (fun x y -> let (o,n) = y in (replace_ann_name_uninterp x o n)) ast vl
  in let ast2 = List.fold_left (fun x y -> let (o,n) = y in (replace_ann_name_uninterp x o n)) ast1 gl
  in let ast3 = List.fold_left (fun x y -> let (f,t) = y in replace_ann_fsym_uninterp x f t) ast2 rl
  in let ast4 = maudeify_ann_names ast3
  in let s' = show_cpf_annotation_wspaces ast4
  in replace_pp_str p s'

(* Filter out a list of all preposts to include only those from a given policy list; note that we *)
(* may change this soon to let Maude do the filtering, but this would mean including the policy languages *)
(* for all defined policies in Maude -- for now we will leave this here *)
let rec filter_pp (pps:prepost list) (fstrs:string list) : (prepost list) =
  List.filter (fun x -> let s = (get_pp_tag x) in List.mem s fstrs) pps

(* Group preconditions, postconditions, modifies clauses, and tinvariants *)
(* into four individual lists. *)
let group_pp (pp:prepost list) : (prepost list * prepost list * prepost list * prepost list)  =
  let rec group_pp_internal (pp:prepost list) (pres:prepost list) (posts:prepost list) 
      (mods:prepost list) (tinvs:prepost list) : (prepost list * prepost list * prepost list * prepost list) =
    match pp with
      | [] -> (pres,posts,mods,tinvs)
      | (PreCond (x,s)) :: ps -> group_pp_internal ps ((PreCond (x,s)) :: pres) posts mods tinvs
      | (PostCond (x,s)) :: ps -> group_pp_internal ps pres ((PostCond (x,s)) :: posts) mods tinvs
      | (Modifies (x,s)) :: ps -> group_pp_internal ps pres posts ((Modifies (x,s)) :: mods) tinvs
      | (TInvariant (x,s)) :: ps -> group_pp_internal ps pres posts mods ((TInvariant (x,s)) :: tinvs)
  in let (l1,l2,l3,l4) = group_pp_internal pp [] [] [] []
  in (List.rev l1, List.rev l2, List.rev l3, List.rev l4)

(* Convert annotation prepost statements in function headers into equivalent instructions in the function body. *)
(* Note that, since type invariants are not included on function bodies, but instead are just on structures, *)
(* we don't handle them here. TODO: Put an exception, in case we are ever asked to do so. *)
let make_init_pp_inst (loc:location) (p:prepost) : instr =
  match p with
    | PreCond (x,s) -> IAssume (x,s,loc)
    | PostCond (x,s) -> IAssert (x,s,loc)
    | Modifies (x,s) -> Unlock (x,s,loc)
    | TInvariant (x,s) -> IAssume (x,"true",loc) (* Need to add error here! *)

(* Convert annotation prepost statements at call sites into equivalent instructions. Note that, since type *)
(* invariants are not included on function bodies, but instead are just on structures, we don't handle them here. *)
(*  TODO: Put an exception, in case we are ever asked to do so. *)
let make_callsite_pp_inst (loc:location) (p:prepost) : instr =
  match p with
    | PreCond (x,s) -> Assert (x,s,loc)
    | PostCond (x,s) -> Assume (x,s,loc)
    | Modifies (x,s) -> Havoc (x,s,loc)
    | TInvariant (x,s) -> IAssume (x,"true",loc) (* Need to add error here! *)

(* Given an annotation string in a prepost, replace a name with an *)
(* uninterpreted string *)
let replace_var (p:prepost) (n:string) (u:string) : prepost =
  let s = get_pp_str p
  in let ast = ann_ast s
  in let ast' = replace_ann_name_uninterp ast n u
  in let s' = show_cpf_annotation_wspaces ast'
  in replace_pp_str p s'

(* Given an annotation string in a prepost, replace a function symbol with an *)
(* uninterpreted string *)
let replace_fsym (p:prepost) (n:string) (u:string) : prepost =
  let s = get_pp_str p
  in let ast = ann_ast s
  in let ast' = replace_ann_fsym_uninterp ast n u
  in let s' = show_cpf_annotation_wspaces ast'
  in replace_pp_str p s'

(* Check equality of annotation prepost statements *)
let pp_equal (p:prepost) (p':prepost) : bool =
  match (p,p') with
    | (PreCond(x,s), PreCond (x',s')) -> x = x' && s = s'
    | (PostCond(x,s), PostCond (x',s')) -> x = x' && s = s'
    | (Modifies(x,s), Modifies(x',s')) -> x = x' && s = s'
    | (TInvariant(x,s), TInvariant(x',s')) -> x = x' && s = s'
    | _ -> false

(* Check equality of an entire list of annotation prepost statements; we don't *)
(* use list map to make it easier to check when, for instance, the lists *)
(* are of differing lengths *)
let rec pp_list_equal (ps:prepost list) (ps':prepost list) : bool =
  match (ps,ps') with
    | ((p::pl),(p'::pl')) -> (pp_equal p p') && (pp_list_equal pl pl')
    | ([],[]) -> true
    | _ -> false

(* Convert a pre- or post-condition into an assert or assume; intended *)
(* for use at call sites. Note that this should never be given a *)
(* type invariant clause; should add an exception throw just in case... *)
let make_pp_inst_inline loc p =
  match p with
    | PreCond (x,s) -> Assert (x,s,loc)
    | PostCond (x,s) -> Assume (x,s,loc)
    | Modifies (x,s) -> Havoc (x,s,loc)
    | TInvariant (x,s) -> Assert (x,"true",loc) (* TODO: should have exception instead *)

(* Print pre- and post-conditions; useful for debugging *)
let print_pp p =
  match p with
    | PreCond (x,s) -> (ignore (E.warn "Found precondition %s:%s\n" x s))
    | PostCond (x,s) -> (ignore (E.warn "Found postcondition %s:%s\n" x s))
    | Modifies (x,s) -> (ignore (E.warn "Found modifies clause %s:%s\n" x s))
    | TInvariant (x,s) -> (ignore (E.warn "Found type invariant clause %s:%s\n" x s))

(* given a function type, get the return type *)
let get_ret_type (t:typ) : typ =
  match t with
    | TFun (rt,_,_,_) -> rt
    | _ -> TVoid([]) (* should put in an exception here, but this case should never happen *)

(* given a function type, get a list of the formal types *)
let get_formals_types (t:typ) : (typ list) =
  match t with
    | TFun (_,Some ftl,_,_) -> List.map (fun x -> let (_,t,_) = x in t) ftl
    | _ -> [ TVoid([]) ] (* should put in an exception here, but this case should never happen *)

(* given a function type, get the function attributes *)
let get_fun_attrs (t:typ) =
  match t with
    | TFun(_,_,_,al) -> al
    | _ -> [] (* should put an exception here, but this case should never happen *)

(* given an attribute list, find all @cpf parameters in the list *)
(* TODO: Fix this to find actual cpf parameters -- removing right now *)
let get_cpf_fromalist (attrs: attribute list) : attribute list = 
  (* (List.map (fun a -> let Attr(s,_) = a in ignore (E.warn "Found attribute (%s)\n" s)) attrs) ; attrs *)
  let fl = List.filter 
    (fun a -> 
       let Attr(s,_) = a in 
	 if (String.length s > 3) then (if (String.sub s 0 4) = "@cpf" then true else false) else false) 
    attrs
  in (*(List.map (fun a -> let Attr(s,_) = a in ignore (E.warn "Found attribute (%s)\n" s)) attrs) ;*) fl


(* given a type, return the @cpf parameters in the type attributes *)
let get_cpf_fromt (t:typ) : attribute list =
  get_cpf_fromalist (typeAttrs t)
  
(* given a type list, return a list of the @cpf parameters for the types *)
let get_cpf_fromtlist (tl:typ list) : attributes list =
  List.map get_cpf_fromt tl

(* add some additional, function-specific attributes, based on the policy; this is done *)
(* in cases where we cannot annotate these in the source file, but TODO should be replaced with *)
(* a cleaner mechanism *)
let add_policy_attrs retann sigann tvlist pplist fname pname loc =
  match fname with
    | "printf" -> 
	let pplist' = ((PostCond (pname,"True")) :: pplist)
	in (retann,sigann,tvlist,pplist')
    | "malloc" ->
	(match pname with
	  | "NOTNULL" -> 
	      let retann' = (Attr("@cpf",[AStr "$null"]) :: retann)
	      in (retann',sigann,tvlist,pplist)
	  | _ -> 
	      let pplist' = ((PostCond (pname,"True")) :: pplist)
	      in (retann,sigann,tvlist,pplist'))
    | "free" -> 
	let pplist' = ((PostCond (pname,"True")) :: pplist)
	in (retann,sigann,tvlist,pplist')
    | _ -> (retann,sigann,tvlist,pplist)

let add_all_globals (f:fundec) (gh:varhash) =
  let vl = getAllVarRecords gh
  in let vl' = List.filter (fun x -> not (inlist f.sglobals x)) vl
  in let vl'' = f.sglobals @ vl' 
  in f.sglobals <- vl'' ; ()

(* Perform inlining of pre- and post-conditions in three places: the start of a function body, for *)
(* function headers; function returns, for postconditions from the header; and call sites, where we *)
(* use information about the called function. *)
class ppInlineVisitor (cgraph: callgraph) (fhash: funhash) (phash:protohash) (ghash:varhash) (cfile: file) = object(self)
  inherit nopCilVisitor
  
  val mutable pres : prepost list = []
  val mutable posts : prepost list = []
  val mutable mods : prepost list = []
  val mutable tinvs : prepost list = [] (* store these for completeness, but we don't use them here for now *)
  val mutable ftype : typ = TVoid([])
  val mutable fresh_num : int = 11 
  val mutable fname : string = ""
  val mutable currentFunction : fundec = Cil.dummyFunDec;

  (* For a function, we need to add the preconditions to the start of the function body, *)
  (* after the declarations. These are added as assumes, since we can assume the preconditions *)
  (* hold at function entry. We also need to change formals in the preconditions and postconditions *)
  (* to their current names -- we could have had a precondition referring to x, but CIL could have *)
  (* renamed this to x_001 or something similar, so we rename the use of the formal inside the *)
  (* precondition to match. Finally, we need to add unlocks for any modifies vars; we will assume *)
  (* that (policy specific) all other formal parameters should be locked to prevent changes and to *)
  (* detect any attempts to modify them *)
  method vfunc (f:fundec) : fundec visitAction =
    (* pr is all preconditions, ps is all postconditions, md is all modifies, ti is all tinvariants *)
    let (pr,ps,md,ti) = (group_pp (filter_pp f.svar.vprepostcond ([!default_policy ; "ALL"])))
    (* used to rename all formals and globals to post-alpha-conversion names *)
    in let rename_formals (p:prepost) : prepost = 
	rename_pp_vars p (List.append f.sformals f.sglobals)
    (* insert info at header: preconditions as assumes, modifies as unlocks, and checkpoint *)
    (* directive, which captures the current state (this is the "old" state later on) *)
    in let updateForPP (f':fundec) : fundec =
	let fbody = f'.sbody.bstmts
	in let firstloc = (* first location in function body *)
	    if fbody == [] then locUnknown
            else get_stmtLoc ((List.hd fbody).skind)
	in let embedPre = (* preconditions, unlocks, and checkpoint *)
	    List.append (List.map (make_init_pp_inst firstloc) pres) 
	      (List.append (List.map (make_init_pp_inst firstloc) mods) ([Checkpoint firstloc]))
	in (f'.sbody.bstmts <- (mkStmt(Instr embedPre) :: fbody); f') (* add new instructions to function body *)
    (* save the info we need later (annotations split into individual vars, type and name info) and *)
    (* then process the children; go ahead and do alpha renaming in annotations here, so we don't need *)
    (* to worry about it elsewhere *)
    in let fplusg = f.sformals @ f.sglobals
    in let maudeifypp pp = maudeify_vars pp fplusg
    in 
      pres <- List.map maudeifypp (List.map rename_formals pr); 
      posts <- List.map rename_formals ps; 
      mods <- List.map maudeifypp (List.map rename_formals md); 
      tinvs <- List.map maudeifypp (List.map rename_formals ti);
      ftype <- f.svar.vtype; fname <- f.svar.vname;
      currentFunction <- f;
      ChangeDoChildrenPost(f,updateForPP)
    
  (* Add postconditions just before a return statement. This is done by replacing @result with *)
  (* an uninterpreted string representing the actual return expression. Note that we don't put vars *)
  (* into Maude form yet, except that they will be in the uninterpreted string, since we will *)
  (* use the CIL d_exp function to generate the expression used for replacement. *)
  (* MAH 12 August 2008: We now also need to handle type annotations, which we will insert as *)
  (* an annotation statement. We won't force this to be an assert, but will handle it separately, *)
  (* so the individual policy can decide. Note that we only insert this when 1) we are returning something *)
  (* in the return statement, i.e. it is of the form return e, not just return, and 2) we have *)
  (* an @cpf attribute. Otherwise, we don't add a check. *)
  method vstmt (s: stmt) : stmt visitAction = 
    let updateForPP (s':stmt) : stmt =
      match s'.skind with
	| Return (e,l) ->
	    let fplusg = currentFunction.sformals @ currentFunction.sglobals
	    in let rpairs = (* If we have a return exp, use this to substitute for @result *)
		(match e with
		   | Some e' -> let str_of_exp = "(@crrnt(" ^ (sprint 200 (d_exp () e')) ^ "))" in [("result",str_of_exp)]
		   | None -> [])
	    in let maudeifypp pp = maudeify_vars_wrepl pp fplusg rpairs
	    in let returnPost = List.map (make_init_pp_inst l) (List.map maudeifypp posts) (* Convert posts to asserts *)
	    in (match e with
		  | None -> (self#queueInstr returnPost) ; s'
		  | Some e' -> (* Also need to check for type annotation on function return type, insert type assert if needed *)
		      let exp_str = "(" ^ (sprint 200 (dn_exp () e')) ^ ")"  
		      in let ret_str = sprint 200 (dn_stmt () s')
		      in let ret_exp_str = exp_str
		      in let cpf_attrs = (get_cpf_fromt (get_ret_type ftype)) @ (get_cpf_fromalist (get_fun_attrs ftype))
		      in let embedTAListInst = TAssert (cpf_attrs, exp_str, ret_str, ret_exp_str, l)
		      in ( match cpf_attrs with
			     | [] -> 
				 (self#queueInstr returnPost); 
				 s'
			     | _ -> 
				 (self#queueInstr (returnPost @ [ embedTAListInst ])); 
				 s'
			 )
	       )
	| _ -> s'
    in 
      match s.skind with 
	  Return _ -> ChangeDoChildrenPost(s,updateForPP)
	| _ -> DoChildren
	    
  (* At a call site, inline preconditions to the called function as assertions, postconditions to *)
  (* the called function as assumes. This way these can be checked/used without actually requiring *)
  (* functions to be called. *)	
  (* MAH 12 August 2008: We now also need to handle type annotations, which we will insert as *)
  (* an annotation statement. We won't force this to be an assert, but will handle it separately, *)
  (* so the individual policy can decide. Note that we only insert this when 1) we are returning something *)
  (* in the return statement, i.e. it is of the form return e, not just return, and 2) we have *)
  (* an @cpf attribute. Otherwise, we don't add a check. *)
  method vinst i = begin
    (* Create a havoc instruction that will havoc all arguments passed as formal parameters *)
    (* and all globals; vlist and glist are the variable names, which are formed into a list *)
    (* as part of the modifies annotation; vgstr is the comma-separated version of this; havocpre *)
    (* is the modifies annotation before substitution; and then the returned instruction is a *)
    (* havoc with the formals replaced with actuals and globals replaced with current global names *)
    let default_havoc l (vl:(string*string)list) (gl:(string*string)list) = 
      let (vlist,_) = List.split vl in
      let (glist,_) = List.split gl in
      let vglist = vlist @ glist in
      let rec comma_sep_list (l:string list) = 
	match l with
	  | (x::y::xs) -> (x ^ "," ^ (comma_sep_list (y::xs)))
	  | (x::xs) -> x
	  | [] -> ""
      in let vgstr = comma_sep_list vglist
      in let havocpre = Modifies (!default_policy,(match vgstr with "" -> "@none" | _ -> vgstr))
      in make_callsite_pp_inst l (maudeify_vars_exps_wrepl havocpre vl gl [])
    (* This is the same as above, except for an unknown function. Since the function is unknown, *)
    (* we don't have the formals, just the list of arguments. We still get the global names, and *)
    (* then we form two comma-separated lists, one of the globals, one of the actuals. havocpre *)
    (* is the modifies with just the globals; havocpre' then puts these into Maude form, doing any *)
    (* needed replacements. The matching code then checks to see how to join the two lists; if we *)
    (* have no args, we just leave it alone; if we have args but no globals, the modifies body is *)
    (* replaced with the actual args list; if we have both, we join them together with a comma *)
    (* between them. We then convert the Modifies annotation into an actual Havoc, like above. *)
    in let default_havoc_unknown l al (gl:(string*string)list) = 
	let (glist,_) = List.split gl in
	let rec comma_sep_list (l:string list) = 
	  match l with
	    | (x::y::xs) -> x ^ "," ^ (comma_sep_list (y::xs))
	    | (x::xs) -> x
	    | [] -> ""
	in let gstr = comma_sep_list glist
	in let astr = comma_sep_list al
	in let havocpre = Modifies (!default_policy,gstr)
	in let havocpre' = maudeify_vars_exps_wrepl havocpre [] gl []
	in let havocpre'' = 
	    match (al, gl) with
	      | ([],[]) -> Modifies (!default_policy,"@none")
	      | ([],_) -> havocpre'
	      | (_,[]) -> replace_pp_str havocpre' astr
	      | _ -> replace_pp_str havocpre' (astr ^ "," ^ (get_pp_str havocpre'))
	in make_callsite_pp_inst l havocpre''
    in let make_warning n s l = Warn (n,s,l)
    in let make_unknown () = Var (makeGlobalVar "@unknown" (TVoid []))
    in let rec make_cpf_unique (cret, csig) =
	let tavreg = Str.regexp "@\\([A-Z][A-Za-z0-9]*\\)"
	in let rec find_all_vars (s:string) (n:int) vset =
	    try 
	      let _ = Str.search_forward tavreg s n
	      in ((*(ignore (E.log "Matched %s\n" (Str.matched_group 1 s))) ; *)
		  (find_all_vars s (Str.match_end ()) (VSet.add (Str.matched_group 1 s) vset)))
	    with Not_found -> vset
	in let vset = List.fold_left 
	    (fun a b -> 
	       let Attr(s,ap) = b in (List.fold_left (fun a b -> match b with AStr s -> find_all_vars s 0 a | _ -> a) a ap)) 
	    VSet.empty (cret @ (List.flatten csig))
	in let vmap = List.fold_left (fun a b -> (*(ignore (E.log "Adding mapping for %s\n" b)) ;*) 
					VMap.add b (fresh_num <- fresh_num + 1 ; b ^ "_" ^ (string_of_int fresh_num)) a) VMap.empty (VSet.elements vset)
	in let make_unique s =
	    VMap.fold (fun k v s -> Str.global_replace (Str.regexp_string (Str.quote "@" ^ k )) ("@" ^ v) s) vmap s
	in let make_unique_attr a =
	    let Attr(s,ap) = a in Attr(s, (List.map (fun a -> match a with AStr s -> AStr (make_unique s) | _ -> a) ap))
	in (List.map make_unique_attr cret, List.map (fun l -> List.map make_unique_attr l) csig, VMap.fold (fun x y s -> ("@('" ^ y ^ ")") :: s) vmap [])
    (* Inline the function call annotation at a call site. lo is an lval option, which contains an lval when the function *)
    (* result is captured (x = f(3), vs f(3), for instance). al is the arg list, l is the location. The formals either come *)
    (* in as a varinfo list, if we have a fundef, or a list of names, if we just have the prototype. Globals come in as a *)
    (* varinfo list or as a string list, like formals, depending on whether we have an actual fundef or a prototype. pplist *)
    (* is the list of annotations that are to be inlined. *)
    in let inline_pp 
	  (fname:string) 
	  (lo: lval option) 
	  al 
	  (l:location)
	  (formals:varinfo list) 
	  (formal_names: string list) 
	  (globals:varinfo list) 
	  (global_names: string list) 
	  (pplist : preposts) 
	  (ftype:typ) 
	  (unknown_fun:bool) 
	  warninst =
	(* Step 1: Filter the pplist to just have those pp's for the policy or for ALL. *)
	let filtered_pp = (filter_pp pplist [!default_policy; "ALL"])
	(* Step 2: Get the return type and parameter types, needed for type annotations. *)
	in let fret = get_ret_type ftype
	in let fsig = get_formals_types ftype
	(* Step 3: If we have type annotation variables, we could have two different functions *)
        (* that use the same var. Make them unique here. *)
	in let (retann,sigann,tvlist) = make_cpf_unique (get_cpf_fromt fret, get_cpf_fromtlist fsig)
        (* Step 4: Functions to create string representations of expressions and lvalues *)
	in let str_of_lval l = "(" ^ (sprint 200 (dn_lval() l)) ^ ")"
	in let str_of_exp e = "(" ^ (sprint 200 (dn_exp () e)) ^ ")"
        (* Step 5: Set up type annotations based on types of formals. If a formal has an annotated type, this leads *)
        (* to an assertion to check to ensure the actual also has that type. *)
	in let paraminsts = 
	    (match unknown_fun with
	       | true -> []
	       | false -> 
		   List.map (fun x -> let ((a,e),f) = x in TAssert (a, (str_of_exp e), (sprint 200 (dn_instr () i)), f, l)) 
		     (List.filter (fun x -> let ((a,_),_) = x in match a with [] -> false | _ -> true) 
			(List.combine (List.combine sigann al) (match formals with [] -> formal_names | _ -> List.map (fun v -> v.vname) formals))))
        (* Step 6: If we used a new type variable here, we need an instruction to declare it. *)
	in let tvlistinst = match tvlist with [] -> [] | _ -> [ TVarDecl (tvlist,l) ]
        (* Step 7: We also include an FCall instruction at the site of the function call, for policies that *)
        (* need to know when control leaves the current function (e.g., it may be okay to violate the external *)
        (* interface in the block, but it may need to be reestablished before a function is called) *)
	in let fcall_inst_list = (match warninst with None -> [FCall (fname,l)] | Some i -> [FCall ("unknown",l) ; i])
	(* Step 8: A hack -- we add any needed policy info to specific designated functions, like malloc or free *)
	in let (retann,sigann,tvlist,filtered_pp) = add_policy_attrs retann sigann tvlist filtered_pp fname !default_policy l
	(* Step 9a: Generate string representations of the actuals *)
	in let al' = List.map (fun x -> "(" ^ (sprint 200 (d_exp () x)) ^ ")") al
        (* Step 9b: Get the names of the formal parameters; note that we use the original name, just in case *)
        (* the name was changed at some point by CIL, since the original name was used in the annotation. *)
	in let fnames = (match formals with [] -> formal_names | _ -> List.map (fun v -> v.vorigname) formals)
        (* Step 9c: Do the same for globals *)
	in let gnames = (match globals with [] -> global_names | _ -> List.map (fun v -> v.vorigname) globals)
        (* Step 9d: Pair up the formal parameters and the actuals *)
        (* TODO: Need to handle varargs functions here; currently will throw an exception *)
	in let fpairs = (match unknown_fun with false -> List.combine fnames al' | true -> [])
        (* Step 9e: Pair up the globals with their current names, put into Maude form *)
	in let gpairs = List.combine gnames 
	    (List.map (fun s -> "ng('" ^ s ^ ")") (match globals with [] -> global_names | _ -> (List.map (fun v -> v.vname) globals)))
        (* Step 9f: If we don't have any annotations on the called function, we will have to havoc all globals *)
        (* in the code we are analyzing, so figure this out here first before we generate the default havoc inst *)
	in let _ = match (filtered_pp, paraminsts, retann) with
	  | ([],[],[]) -> add_all_globals currentFunction ghash
	  | _ -> ()
	(* Step 9g: Do the same for the current function's globals, which we will need in the havoc instruction *)
	in let gpairs' = (List.combine 
			    (List.map (fun v -> v.vorigname) currentFunction.sglobals)
			    (List.map (fun v -> ((maudeNamePrefix v) ^ "('" ^ v.vname ^ ")")) currentFunction.sglobals))
	(* Step 10: Create a havoc all instruction, just in case we need it *)
	in let havocinst = (match unknown_fun with
			      | false -> default_havoc l fpairs gpairs'
			      | true -> default_havoc_unknown l al' gpairs')
        (* Step 11: Now, we need to actually do the inlining based on the filter pp list and any annotations *)
        (* Note that we don't cover all cases, since sometimes it doesn't matter if we differentiate between them *)
        (* or not. For instance, if we don't have any annotations at all, we need to assume that the function could *)
	(* change anything. Once we have at least one, we assume the function has been fully annotated. *)
	in match (filtered_pp, paraminsts, retann,lo) with
          (* First case: no pp annotations, no type annotations, no return value; havoc all *)
	  | ([],[],[],None) -> ChangeTo (fcall_inst_list @ [havocinst ;
				make_warning 2 ("No annotations on function " ^ fname ^ ", all actuals and globals will be havoc'ed") l])

          (* Second case: no pp annotations, no type annotations, has a return value; havoc all, return unknown *)
	  | ([],[],[],Some lv) ->  ChangeTo ( fcall_inst_list @ [havocinst;
				make_warning 2 ("No annotations on function " ^ fname ^ ", all actuals and globals will be havoc'ed") l ;
				(Set (lv, Lval (make_unknown(), NoOffset), l)) ])

          (* Third case: no pp annotations, no return type annotation, param annotations, no return value *)
	  | ([],_,[],None) -> ChangeTo ( tvlistinst @ paraminsts @ fcall_inst_list )

          (* Fourth case: no pp annotations, no return type annotation, param annotations, has a return value, so return unknown *)
	  | ([],_,[],Some lv) -> ChangeTo ( tvlistinst @ paraminsts @ fcall_inst_list @ [ (Set (lv, Lval (make_unknown(), NoOffset), l))])

          (* Fifth case: no pp annotations, has return type annotations, may have param annotations, no return value *)
	  | ([],_,_,None) -> ChangeTo ( tvlistinst @ paraminsts @ fcall_inst_list )

          (* Sixth case: no pp annotations, has return type annotations, may have param annotations, has a return value so insert tassume *)
	  | ([],_,_,Some lv) -> ChangeTo ( tvlistinst @ paraminsts @ fcall_inst_list @
						[TAssume (retann, (str_of_lval lv), (sprint 200 (dn_instr () i)), "return value", l)])

	  (* Seventh case: at least 1 pp annotation: need to take next steps based on which kinds of pp annotations we have *)
	  | (_,_,_,_) ->
              (* Prepare any replacement pairs needed during renaming *)
	      let rpairs = (* If we assign the result of the call, use the receiver to substitute for @result *)
		  (match lo with
		     | Some lv -> let str_of_exp = (sprint 200 (d_exp () (Lval lv))) in [("result",str_of_exp)]
		     | None -> [])
	      (* Now, perform the renaming to put annotations into a proper maude-ified format *)
	      in let maudeifypp pp = maudeify_vars_exps_wrepl pp fpairs gpairs rpairs
	      in let (pp':prepost list) = List.map maudeifypp filtered_pp
	      (* Next, group the pps into pre, post, mod, tinv *)
	      in let (pr,po,mo,ti) = group_pp pp'
	      (* Then, convert them into the appropriate instructions *)
	      in let make_callsite_loc_pp_inst ps = List.map (make_callsite_pp_inst l) ps
	      in let (asrt,asum,hav,ti) = (make_callsite_loc_pp_inst pr,
					 make_callsite_loc_pp_inst po,
					 make_callsite_loc_pp_inst mo,
					 make_callsite_loc_pp_inst ti)
              (* Now, we can insert the instructions. If we have no assumes and no return annotations, we will want *)
              (* to "return" unknown; otherwise, we will use either one or the other. Note that we have at least one *)
              (* annotation, so we can assume the function is completely annotated. Lack of a modifies clause on an *)
              (* otherwise annotated function is equivalent to saying that nothing is modified. *)
	      in (match (asum, retann,lo) with
		    | (_,_,None)      -> ChangeTo ( tvlistinst @ paraminsts @ asrt @ fcall_inst_list @ hav @ asum)
		    | ([],[],Some lv) -> ChangeTo ( tvlistinst @ paraminsts @ asrt @ 
						      fcall_inst_list @ [ (Set (lv, Lval (make_unknown(), NoOffset), l)) ] @ hav)
		    | ([],_,Some lv)  -> ChangeTo ( tvlistinst @ paraminsts @ asrt @ fcall_inst_list @ hav @ 
						     [ TAssume (retann, (str_of_lval lv), (sprint 200 (dn_instr () i)), "return value", l) ])
		    | (_,[],Some lv)  -> ChangeTo ( tvlistinst @ paraminsts @ asrt @ fcall_inst_list @ hav @ asum )
		    | (_,_,Some lv)   -> ChangeTo ( tvlistinst @ paraminsts @ asrt @ fcall_inst_list @ hav @ asum @ 
						     [ TAssume (retann, (str_of_lval lv), (sprint 200 (dn_instr () i)), "return value", l) ])
		 )
    in let inline_fun (fname:string) (lo: lval option) al l (fdef:fundec) warninst =
	inline_pp fname lo al l fdef.sformals [] fdef.sglobals [] fdef.svar.vprepostcond fdef.svar.vtype false warninst 
    in let inline_proto (fname:string) (lo: lval option) al l (vdef:varinfo) warninst =
	inline_pp fname lo al l [] vdef.ppargs [] vdef.ppglobals vdef.vprepostcond vdef.vtype false warninst
    (* TODO: We may want to pass the function type as well; in unknown cases, we may be able to use *)
    (* type information from the pointer type, even if we don't have pre or post-conditions. *)
    in let handle_call (lo: lval option) al l (fname:string) (unknown:bool) warninst =
	match unknown with
	  | false ->
	      (* First, try to get back the function def from the function hash. *)
	      let fdef = getFunByName fhash fname in (
		  match fdef with
		    | Some fd -> 
			inline_fun fname lo al l fd warninst (* Found it -- inline it *)
			  
		    | None -> (* Didn't find it -- see if we have a prototype instead *)
			(let vdef = getProtoByName phash fname in 
			   match vdef with
			     | Some vd -> 
				 inline_proto fname lo al l vd warninst (* Found a prototype -- inline it *)			       
			     | None -> 
				 inline_pp fname lo al l [] [] [] [] [] (TVoid([])) true warninst (* Not even a prototype *)
			)
		)
	  | true -> inline_pp fname lo al l [] [] [] [] [] (TVoid([])) true warninst (* Unknown function *)
    in 
      match i with
	(* Case 1: Standard function call, like f(x) *)
	| Call(lo,Lval(Var callee, NoOffset),al,l) ->
	    handle_call lo al l callee.vname false None

        (* Case 2: Function call through a function pointer. This handles the case where we can resolve the *)
	(* pointer to one specific function. Note that this is influenced by a flag saying whether we are *)
        (* performing alias analysis; if we are using a truly modular approach, we may not be able to say *)
        (* anything at all about the pointer, in which case we have to assume it could point to anything. *)
        (* TODO: Add support for cases where we can narrow this down to multiple known functions. If they all *)
        (* have the same interface, we can treat it just like the single function case. If they have different *)
        (* interfaces, we may still be able to handle it, depending on how wildly the interfaces differ. *)
	| Call(lo,Lval (Mem callee, NoOffset),al,l) ->
	    if !use_aa = true then 
	      let flist = P.resolve_funptr callee
	      in if (List.length flist) = 1 
		then 
		  let called_fun = List.hd flist
		  in handle_call lo al l called_fun.svar.vname false None
		else
		  (* Function pointer could point to multiple targets; havoc all, since we don't know what the function may actually change *)
		  handle_call lo al l "" true (Some (make_warning 1 "Unable to determine function used at call site; function pointer could point to multiple targets." l))
	    else 
	      (* Function pointer, but no alias analysis; havoc all, since we don't know what the function may actually change *)
	      handle_call lo al l "" true (Some (make_warning 1 "Unable to determine function used at call site, alias analysis disabled." l))

        (* A catch-all case. Need to figure out if the above two are sufficient -- they may not capture function pointers stored *)
        (* in arrays or structures, for instance, which would then need to be handled here. *)
	| Call(lo,e,al,l) ->
	    (* An unhandled call -- need to insert a warning; plus havoc all, since we don't know what the function may actually change *)
	      handle_call lo al l "" true (Some (make_warning 1 "Unhandled call case." l))
	      
        (* We are only concerned with calls -- for all other instructions, just perform default behavior. *)
	| _ -> DoChildren
  end
    
end

let processInline (f:file) = begin
  let cg = computeGraph f in
  let fh,ph = computeFunHash f in 
  let gh = computeFindGlobals f in
  let obj = new ppInlineVisitor cg fh ph gh f in
    begin
      (* P.analyze_file f; *)
      visitCilFileSameGlobals (obj :> cilVisitor) f;
    end
end

let feature : featureDescr = 
  { fd_name = "ppinline";
    fd_enabled = ref true;
    fd_description = "inline pre- and post-conditions, removing function calls";
    fd_extraopt = [
      ("--pp_default_policy",
       Arg.String (fun s -> default_policy := s),
       " Specify the default policy");
      ("--pp_use_aa",
       Arg.Unit (fun _ -> use_aa := true),
       "Allow use of alias analysis during inlining (requires whole program analysis)");
    ];
    fd_doit = 
      (function (f: file) -> processInline f);
    fd_post_check = true;
  } 


