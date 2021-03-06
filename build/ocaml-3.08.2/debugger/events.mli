(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: events.mli,v 1.3 1999/11/17 18:57:24 xleroy Exp $ *)

open Instruct

(** Current events. **)

(* The event at current position. *)
val current_event : debug_event option ref

(* Recompute the current event *)
val update_current_event : unit -> unit

(* Current position in source. *)
(* Raise `Not_found' if not on an event (beginning or end of program). *)
val current_point : unit -> string * int

val current_event_is_before : unit -> bool

