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

module StringSet = Set.Make(String)

(* This mutable global variable stores the current context. *)
let current =
  ref StringSet.empty

(* This declares [id] as a typedef name. *)
let declare_typedefname id =
  current := StringSet.add id !current

(* This declares [id] as a variable (hence, un-declares it as a typedef name). *)
let declare_varname id =
  current := StringSet.remove id !current

(* This tests whether [id] is known as a typedef name. *)
let is_typedefname id =
  StringSet.mem id !current

(* A context is just a set of identifiers. It is the set of typedef
   names that are now visible. *)
type context =
  StringSet.t

(* This takes a snapshot of the current context. *)
let save_context () =
  !current

(* This re-installs a snapshot as the current context. *)
let restore_context snapshot =
  current := snapshot
