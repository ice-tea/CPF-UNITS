(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*    Valerie Menissier-Morain, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id: string_misc.ml,v 1.4 2001/12/07 13:40:16 xleroy Exp $ *)

let rec index_char str chr pos =
  if pos >= String.length str then -1
  else if String.get str pos = chr then pos
  else index_char str chr (pos + 1)
;;
