(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dll.mli,v 1.5 2002/07/02 16:13:12 weis Exp $ *)

(* Handling of dynamically-linked libraries *)

(* Extract the name of a DLLs from its external name (xxx.so or -lxxx) *)
val extract_dll_name: string -> string

(* Open a list of DLLs, adding them to opened_dlls.
   Raise [Failure msg] in case of error. *)
val open_dlls: string list -> unit

(* Close all DLLs *)
val close_all_dlls: unit -> unit

(* The abstract type representing C function pointers *)
type dll_address

(* Find a primitive in the currently opened DLLs and return its address.
   Raise [Not_found] if not found. *)
val find_primitive: string -> dll_address

(* If linking in core (dynlink or toplevel), synchronize the VM
   table of primitive with the linker's table of primitive
   by storing the given primitive function at the given position
   in the VM table of primitives.  *)
val synchronize_primitive: int -> dll_address -> unit

(* Add the given directories at the head of the search path for DLLs *)
val add_path: string list -> unit

(* Initialization for separate compilation.
   Initialize the DLL search path to the directories given in the
   environment variable CAML_LD_LIBRARY_PATH, plus contents of ld.conf file
   if argument is [false].  If argument is [true], ignore ld.conf. *)
val init_compile: bool -> unit

(* Initialization for linking in core (dynlink or toplevel).
   Initialize the search path to the same path that was used to start
   the running program (CAML_LD_LIBRARY_PATH + directories in executable +
   contents of ld.conf file).  Take note of the DLLs that were opened
   when starting the running program. *)
val init_toplevel: string -> unit

