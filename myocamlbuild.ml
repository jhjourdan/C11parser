(* **********************************************************************)
(*                                                                      *)
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

open Ocamlbuild_plugin
open Command

let () =
  dispatch (function After_rules ->
    (* Nazi warnings *)
    flag ["ocaml"; "compile"] (S[A "-w"; A "@1..49-4-9-41-44"])
  | _ -> ()
  )
