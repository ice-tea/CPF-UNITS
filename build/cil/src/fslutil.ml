open Escape
open Pretty
open Trace      (* sm: 'trace' function *)
open Cil
open Cpfannotation
module L = Cpflex
module P = Cpfparse
module E = Errormsg

(* The substring of string s, starting at n and ending at the end of s *)
let sub_to_end (s:string) (n:int) : string =
  let slen = String.length s
  in String.sub s n (slen - n)

(* Determine if a string contains a Maude id marker -- n('id) -- starting *)
(* at a given location loc and occuring before location loc + len *)
let contains_id_marker (s:string) (loc:int) (len:int) : bool =
  let namereg = Str.regexp "n[tfg]?('" in 
    try
      let nloc = Str.search_forward namereg s loc in
	if nloc <= loc + (len - 1) then true else false
    with 
      | Not_found -> false


(* given a regexp r, replacement template t, location loc, and string s, try to find a match for r in s *)
(* starting at loc; if we find one, replace the matched string using replacement template t *)
let match_replace_arbitrary (r:Str.regexp) (t:string->string) (loc:int) (s:string) : (string * int * int) =
  try 
    let mloc = Str.search_forward r s loc in (* matched location *)
    let mstr = Str.matched_string s in (* matched substring *)
      match (contains_id_marker s mloc (String.length mstr)) with
	| false -> (* valid match, continue with substitution *)
	    begin
	      let mlen = String.length mstr in (* length of match *)
	      let rstr = Str.replace_matched (t mstr) s in (* new substring, based on replacement template *)
	      let finalstr = (String.sub s 0 mloc) ^ rstr ^  (sub_to_end s (mloc+mlen)) (* insert the replacement into same location as the original *)
	      in finalstr,mloc,String.length rstr (* return new string, start of match, and length of replacement *)
	    end
	| true -> (* attempting to match the id marker -- just skip past it, but continue trying to find matches *)
	    (s,mloc,String.length mstr)
  with
    | Not_found -> (s,0,0) (* no matches found -- just return the original string with 0's to represent this *)

(* Using a set of regexp/replacement template/function triples rs and a string s, perform the replacements *)
(* in the templates based on matches to the regexps in s until no more replacements can be found. *)	
let bulk_replace_string (rs : (Str.regexp * (string -> string) * (Str.regexp -> (string -> string) -> int -> string -> (string * int * int))) list) (s:string) : string =

  (* run through a list of regexp/template pairs, processing each one over the string until we get no more hits *)
  let rec process_all (rs : (Str.regexp * (string -> string) * (Str.regexp -> (string -> string) -> int -> string -> (string * int * int))) list) (loc:int) (s:string)
      : string =
    match rs with
      | [] -> s
      | ((r,t,f) :: rs') ->
	  let (s',spos,slen) = f r t loc s
	  in if (spos = 0 && slen = 0) then (* no match *)
	      process_all rs' 0 s'
	    else
	      if (((String.length s') - 1) > (spos + slen)) then (* there is string left after the match *)
		process_all ((r,t,f) :: rs') (spos+slen) s'
	      else
		process_all rs' 0 s' (* the match was at the end of the string *)
		  
  in process_all rs 0 s (* start processing at beginning of string *)

(* COMMON REGEXP/TEMPLATE PAIRS *)
let c_struct_wdot = Str.regexp "\\.[ ]*[A-Za-z][A-Za-z0-9_]*" , (fun vn -> " . n('" ^ (sub_to_end vn 1) ^ ")")
let c_struct_wptr = Str.regexp "->[ ]*[A-Za-z][A-Za-z0-9_]*" , (fun vn -> " -> n('" ^ (sub_to_end vn 2) ^ ")")
let c_ptr_deref = Str.regexp "\\*[ ]*[A-Za-z][A-Za-z0-9_]*", (fun vn -> " * n('" ^ (sub_to_end vn 1) ^ ")")
let c_var_name = Str.regexp "[A-Za-z][A-Za-z0-9_]*", (fun vn -> " n('" ^ vn ^ ") ")

(* Function that, given a replacement function and a regexp/template pair, returns a new triple *)
let make_replace_triple (p : Str.regexp * (string -> string)) (f : Str.regexp -> (string -> string) -> int -> string -> (string * int * int)) 
    : (Str.regexp * (string -> string) * (Str.regexp -> (string -> string) -> int -> string -> (string * int * int))) =
  match p with
    | (r,t) -> r,t,f

