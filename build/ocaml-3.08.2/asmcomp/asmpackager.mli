(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: asmpackager.mli,v 1.1 2002/02/08 16:55:30 xleroy Exp $ *)

(* "Package" a set of .cmx/.o files into one .cmx/.o file having the
   original compilation units as sub-modules. *)

val package_files: Format.formatter -> string list -> string -> unit

type error =
    Illegal_renaming of string * string
  | Forward_reference of string * string
  | Linking_error
  | Assembler_error of string
  | File_not_found of string
  | No_binutils

exception Error of error

val report_error: Format.formatter -> error -> unit
