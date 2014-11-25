open Cil
open Trace
open Printf
module P = Pretty
module IH = Inthash
module H = Hashtbl
module E = Errormsg
module C = Cpfannotation
module U = Fslutil

type funhash = (string, fundec) Hashtbl.t
type protohash = (string, varinfo) Hashtbl.t

(* Retrieve a function by name from a function hash table *)
let getFunByName (fhash:funhash) (s:string) : fundec option = 
  try
    let res = H.find fhash s in Some res
  with Not_found -> 
    None

(* Retrieve a function prototype by name from a prototype hash table *)
let getProtoByName (phash:protohash) (s:string) : varinfo option =
  try
    let res = H.find phash s in Some res
  with Not_found -> 
    None

(* Build the function and prototype hash tables *)
class fhComputer (fh: funhash) (ph:protohash) = object(self)
  inherit nopCilVisitor

  (* For each global, check to see if it is a function prototype; if it is, add it into *)
  (* the function prototype hash. *)
  method vglob (g:global) : global list visitAction = begin
    match g with
      | GVarDecl (v,l) ->
	  if (isFunctionType v.vtype) then H.add ph v.vname v; (* Add to hash *)
	  DoChildren
      | _ -> DoChildren
  end

  (* Add each function to the function hash and flag each parameter as a formal *)
  (* in the association varinfo record. *)
  method vfunc (f:fundec) : fundec visitAction = begin 
    f.sformals <-  List.map (fun v -> v.vformal <- true ; v) f.sformals; (* Mark as formal parameters *)
    (trace "funhash" (P.dprintf "cataloging function %s\n" f.svar.vname));
    H.add fh f.svar.vname f; (* Add to hash *)
    SkipChildren; (* No need to process function bodies *)
  end

end

let computeFunHash (f:file) : funhash * protohash = begin
  let fh = H.create 37 in 
  let ph = H.create 37 in 
  let obj:fhComputer = new fhComputer fh ph in

  (* visit the whole file, computing the function and prototype hash tables *)
  visitCilFileSameGlobals (obj :> cilVisitor) f;

  (* return the computed function and prototype hash tables *)
  fh,ph
end

let doFunHash = ref false

let feature : featureDescr = 
  { fd_name = "funhash";
    fd_enabled = doFunHash;
    fd_description = "generation of a hash table containing all function definitions";
    fd_extraopt = [];
    fd_doit = 
    (function (f: file) -> 
      let _ = computeFunHash f in ());
    fd_post_check = false;
  } 
