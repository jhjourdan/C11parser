(* **********************************************************************)
(*                                                                      *)
(*          Jacques-Henri Jourdan, INRIA Paris                          *)
(*          François Pottier, INRIA Paris                               *)
(*                                                                      *)
(*  Copyright Institut National de Recherche en Informatique et en      *)
(*  Automatique.  All rights reserved.  This file is distributed        *)
(*  under the terms of the GNU General Public License as published by   *)
(*  the Free Software Foundation, either version 2 of the License, or   *)
(*  (at your option) any later version.  This file is also distributed  *)
(*  under the terms of the INRIA Non-Commercial License Agreement.      *)
(*                                                                      *)
(* **********************************************************************)

(* This declares [id] as a typedef name. *)
val declare_typedefname: string -> unit

(* This declares [id] as a variable (hence, un-declares it as a typedef name). *)
val declare_varname: string -> unit

(* This tests whether [id] is known as a typedef name. *)
val is_typedefname: string -> bool

(* A context is just a set of identifiers. It is the set of typedef
   names that are now visible. *)
type context

(* This takes a snapshot of the current context. *)
val save_context: unit -> context

(* This re-installs a snapshot as the current context. *)
val restore_context: context -> unit
