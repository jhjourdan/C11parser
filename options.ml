(* **********************************************************************)
(*                                                                      *)
(*          Jacques-Henri Jourdan, INRIA Paris-Rocquencourt             *)
(*                                                                      *)
(*  Copyright Institut National de Recherche en Informatique et en      *)
(*  Automatique.  All rights reserved.  This file is distributed        *)
(*  under the terms of the GNU General Public License as published by   *)
(*  the Free Software Foundation, either version 2 of the License, or   *)
(*  (at your option) any later version.  This file is also distributed  *)
(*  under the terms of the INRIA Non-Commercial License Agreement.      *)
(*                                                                      *)
(* **********************************************************************)

(* [c99_scoping] is [true] if using C99/C11 scoping rules, [false] if
   using ANSI scoping rules. *)
(* Should be set to [true] for C99/C11 parsing, [false] for C89/C90
   parsing. *)
let c99_scoping = true

(* [atomic_strict_syntax] is [true] if [_Atomic] followed by an
   opening parenthesis is always a type specifier. *)
(* Should be set to [true] for strictly compliant C parsing, but
   setting it to [false] only accepts more programs (and no program can
   be wrongly parsed because of that). *)
let atomic_strict_syntax = false

(* [allow_implicit_int] is [true] if the parser accepts declarations
   without type specifier (as in the ANSI C89 standard). The grammar
   of declarations has to be modified a lot, so we use a different
   grammar for this mode. *)
(* Should be set to [true] for C99/C11 parsing, [false] for C89/C90
   parsing. *)
let allow_implicit_int = false
