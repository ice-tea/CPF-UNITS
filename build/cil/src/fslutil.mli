(* The substring of string s, starting at n and ending at the end of s *)
val sub_to_end : string -> int -> string

(* Determine if a string contains a Maude id marker -- n('id) -- starting *)
(* at a given location loc and occuring before location loc + len *)
val contains_id_marker : string -> int -> int -> bool

(* given a regexp r, replacement template t, location loc, and string s, try to find a match for r in s *)
(* starting at loc; if we find one, replace the matched string using replacement template t *)
val match_replace_arbitrary : Str.regexp -> (string -> string) -> int -> string -> (string * int * int)

(* Using a set of regexp/replacement template/function triples rs and a string s, perform the replacements *)
(* in the templates based on matches to the regexps in s until no more replacements can be found. *)
val bulk_replace_string : (Str.regexp * (string->string) * (Str.regexp -> (string -> string) -> int -> string -> (string * int * int))) list -> string -> string

(* COMMON REGEXP/TEMPLATE PAIRS *)
val c_struct_wdot : Str.regexp * (string -> string)
val c_struct_wptr : Str.regexp * (string -> string)
val c_ptr_deref : Str.regexp * (string -> string)
val c_var_name : Str.regexp * (string -> string)

(* Function that, given a replacement function and a regexp/template pair, returns a new triple *)
val make_replace_triple : (Str.regexp * (string -> string)) -> (Str.regexp -> (string -> string) -> int -> string -> (string * int * int)) -> 
  (Str.regexp * (string -> string) * (Str.regexp -> (string -> string) -> int -> string -> (string * int * int)))
