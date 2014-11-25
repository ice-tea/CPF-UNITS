open Cil
open Trace
open Printf
open Cpfannotation
open Fslutil
open Cpfutil
module P = Pretty
module IH = Inthash
module H = Hashtbl
module E = Errormsg
module F = Funhash
module T = Ptranal

let use_aa = ref false

type varhash = (string, varinfo) Hashtbl.t

(* True if v \in l, false otherwise (comparison based on variable name) *)
(* Note that this should be optimized if we expect there to be many globals, as *)
(* this is just a simple linear search *)
let rec inlist (l:varinfo list) (v:varinfo) : bool =
  match l with
    | x :: xs -> if (v.vname = x.vname) then true else (inlist xs v)
    | [] -> false

(* Retrieve a variable by name from a var hash table *)
let getVarByName (vhash:varhash) (s:string) : varinfo option = 
  try
    let res = H.find vhash s in Some res
  with Not_found -> 
    None

(* Retrieve the names of all variables in the hash table, as a list; make *)
(* sure each name appears only once *)
let getAllVarNames (vhash:varhash) : string list =
  let l1 = H.fold (fun x y c -> x :: c) vhash [] in
    List.fold_left (fun x y -> if  List.mem y x then x else (y::x)) [] l1

(* Retrieve the varinfo records of all variables in the hash table, as a list; *)
(* make  sure each name appears only once *)
let getAllVarRecords (vhash:varhash) : varinfo list =
  let l1 = H.fold (fun x y c -> y :: c) vhash [] in
    List.fold_left (fun x y -> if inlist x y then x else (y::x)) [] l1

(* Find all globals used in each function, either directly in C code or *)
(* inside an annotation. *)
class fgComputer (fh: F.funhash) (ph:F.protohash) (gh:varhash) (cfile: file) = object(self)
  inherit nopCilVisitor

  val mutable currentFunction : fundec = Cil.dummyFunDec;
  val mutable infunction : bool = false;
  val mutable globalsSet : bool = false;

  (* Visit each prototype. We don't know what globals the function uses internally, *)
  (* but we can see which ones are referenced in any annotations and add them to the . *)
  (* information we keep for the prototype. *)
  (* TODO: find_ann_globals is copied from below, should factor this out *)
  method vglob (g:global) : global list visitAction = begin
    if (not globalsSet) then
      begin
	let _ = 
	  List.map (fun v -> H.add gh v.vorigname v)
	    (List.fold_left (fun x y ->
			       match y with
				 | GVarDecl (v,l) ->
				     if ((not (isFunctionType v.vtype)) && v.vglob) 
				     then (v :: x)
				     else x
				 | GVar (v,_,_) ->
				     if ((not (isFunctionType v.vtype)) && v.vglob) 
				     then (v :: x)
				     else x
				 | _ -> x
			    ) [] cfile.globals)
	in globalsSet <- true
      end ;
    match g with
      | GVarDecl (v,l) ->
	  if (isFunctionType v.vtype) then (
	    let find_ann_globals ppl frmls =
	      let find_ann_globals_aux pp =
		let annstr = (
		  match pp with
		    | PreCond (x,s) -> s
		    | PostCond (x,s) -> s
		    | Modifies (x,s) -> s
		    | TInvariant (x,s) -> s
		) 
		in let annast = ann_ast annstr
		in let varlist = get_ann_name_list annast
		in List.filter (fun x -> 
				  try 
				    let _ = H.find gh x in (if List.mem x frmls then false else true)
				  with Not_found -> false) varlist
	      in List.fold_left (fun x y -> (find_ann_globals_aux y) @ x) [] ppl
	    in let vl = find_ann_globals v.vprepostcond v.ppargs
	    in let vl' = List.filter (fun x -> not (List.mem x v.ppglobals)) vl
	    in v.ppglobals <- (vl' @ v.ppglobals) ; DoChildren)
	  else DoChildren
      | _ -> DoChildren
  end

  (* Visit each function: we will check to see which globals each function uses. *)
  (* A function uses a global if either: *)
  (*   1. the function body refers to the global in C code *)
  (*   2. an annotation in the function references a global *)
  (* This information can be used during analysis and during verification task *)
  (* generation, to, for instance, add declarations for all globals to the start *)
  (* of a function. *)
  (* TODO: find_ann_globals is a duplicate of the same function in vinst, should *)
  (* factor this out. *)
  method vfunc (f:fundec) : fundec visitAction = begin 
    let resetFlags (f':fundec) =
      currentFunction <- Cil.dummyFunDec;
      infunction <- false;
      f'
    in let find_ann_globals ppl frmls =
	let find_ann_globals_aux pp =
	  let annstr = (
	    match pp with
	      | PreCond (x,s) -> s
	      | PostCond (x,s) -> s
	      | Modifies (x,s) -> s
	      | TInvariant (x,s) -> s
	  ) 
	  in let annast = ann_ast annstr
	  in let varlist = get_ann_name_list annast
	  in List.filter (fun x -> 
			    try 
			      let _ = H.find gh x in (if List.mem x frmls then false else true)
			    with Not_found -> false) varlist
	in List.fold_left (fun x y -> (find_ann_globals_aux y) @ x) [] ppl
    (* The first time we do this, get back all the global vars as a hash table so *)
    (* we can easily look them up later. *)
    in if (not globalsSet) then
	begin
	  let _ = 
	    List.map (fun v -> H.add gh v.vorigname v)
	      (List.fold_left (fun x y ->
				 match y with
				   | GVarDecl (v,l) ->
				       if ((not (isFunctionType v.vtype)) && v.vglob) 
				       then (v :: x)
				       else x
				   | GVar (v,_,_) ->
				       if ((not (isFunctionType v.vtype)) && v.vglob) 
				       then (v :: x)
				       else x
				   | _ -> x
			      ) [] cfile.globals)
	  in globalsSet <- true
	end ;
      currentFunction <- f;
      infunction <- true;
      (* Look in this function's annotations as well to see if it references any globals *)
      (* that are not actually used in the function itself. *)
      let vl = find_ann_globals f.svar.vprepostcond (List.map (fun x -> x.vorigname) f.sformals)
      in let _ = List.map (fun x -> 
			     try
			       let v = H.find gh x
			       in if not (inlist f.sglobals v) then (f.sglobals <- (v :: f.sglobals))
			     with Not_found -> ()
			  ) vl
      in (trace "findglobals" (P.dprintf "searching function %s\n" f.svar.vname));
	ChangeDoChildrenPost(f,resetFlags)
  end

  (* Look for all function calls. If we find one, we need to look at the annotations *)
  (* on the function to see if any globals are referenced. Note that we filter out *)
  (* formals in case a global is masked by a formal; i.e., int x, then int f(int x){ ... } *)
  (* would make a reference to x in an annotation be a reference to the formal, NOT the *)
  (* global variable. *)
  method vinst i = begin
    let find_ann_globals ppl frmls =
      let find_ann_globals_aux pp =
	let annstr = (
	  match pp with
	    | PreCond (x,s) -> s
	    | PostCond (x,s) -> s
	    | Modifies (x,s) -> s
	    | TInvariant (x,s) -> s
	) 
	in let annast = ann_ast annstr
	in let varlist = get_ann_name_list annast
	in List.filter (fun x -> 
			  try 
			    let _ = H.find gh x in (if List.mem x frmls then false else true)
			  with Not_found -> false) varlist
      in List.fold_left (fun x y -> (find_ann_globals_aux y) @ x) [] ppl
    in let find_fun_ann_globals (fname:string) : (string list) =
      (* First, get the function back from the function or prototype hash *)
	let (fdo,vio) = (F.getFunByName fh fname, F.getProtoByName ph fname)
	in match (fdo,vio) with
	  | (Some fd, _) -> find_ann_globals fd.svar.vprepostcond (List.map (fun x -> x.vorigname) fd.sformals)
	  | (_, Some vi) -> find_ann_globals vi.vprepostcond vi.ppargs
	  | _ -> [] (* We don't handle this here, but do at inlining *)
    in 
      match i with
	| Call(lo,Lval(Var callee, NoOffset),al,l) -> (* This is a standard function call *)
	    let vl = find_fun_ann_globals callee.vname
	    in let _ = List.map (fun x -> 
				   try
				     let v = H.find gh x
				     in if not (inlist currentFunction.sglobals v) then (currentFunction.sglobals <- (v :: currentFunction.sglobals))
				   with Not_found -> ()
				) vl
	    in DoChildren
	| Call(lo,Lval (Mem callee, NoOffset),al,l) -> (* This is a function call through a function pointer *)
	    if !use_aa then (
	      let flist = T.resolve_funptr callee
	      in if (List.length flist) = 1 
		then 
		  let called_fun = List.hd flist
		  in let vl = find_fun_ann_globals called_fun.svar.vname
		  in let _ = List.map (fun x -> 
					 try
					   let v = H.find gh x
					   in if not (inlist currentFunction.sglobals v) then (currentFunction.sglobals <- (v :: currentFunction.sglobals))
					 with Not_found -> ()
				      ) vl
		  in ()
	    ) ; DoChildren
	| Call _ -> (* This is a function call that we have no information about *) 
	    DoChildren (* We won't do anything here, but this most likely will result in a havoc @all *)
	| _ -> DoChildren
  end

  method vvrbl (v:varinfo) : varinfo visitAction = begin
    (if infunction then 
      if v.vglob then 
	match v.vtype with 
	  | TVoid _ | TInt _ | TFloat _ | TPtr _ | TArray _ | TComp _ | TEnum _  | TNamed _ -> 
	      if not (inlist currentFunction.sglobals v) then (currentFunction.sglobals <- (v :: currentFunction.sglobals))
	  | TFun _ | TBuiltin_va_list _ -> ());
    SkipChildren
  end
end

let processFindGlobals (f:file) = begin
  let fh,ph = F.computeFunHash f in 
  let gh = H.create 37 in
  let obj = new fgComputer fh ph gh f in
    visitCilFileSameGlobals (obj :> cilVisitor) f
end

let computeFindGlobals (f:file) : varhash = begin
  let fh,ph = F.computeFunHash f in 
  let gh = H.create 37 in
  let obj = new fgComputer fh ph gh f in
    visitCilFileSameGlobals (obj :> cilVisitor) f ; gh
end

let doFindGlobals = ref false

let feature : featureDescr = 
  { fd_name = "findglobals";
    fd_enabled = doFindGlobals;
    fd_description = "add the list of globals used in a function to each fundec record";
    fd_extraopt = [
      ("--fg_use_aa",
       Arg.Unit (fun _ -> use_aa := true),
       "Allow use of alias analysis in findglobals (requires whole program analysis)");
    ];
    fd_doit = 
      (function (f: file) -> processFindGlobals f);
    fd_post_check = false;
  } 
