(**************************************************************************)
(*                    Jacques-Henri Jourdan, Inria Paris                  *)
(*                      François Pottier, Inria Paris                     *)
(*                                                                        *)
(*  Copyright Inria. All rights reserved. This file is distributed under  *)
(*  the terms of the GNU General Public License as published by the Free  *)
(*  Software Foundation, either version 2 of the License, or (at your     *)
(*  option) any later version.                                            *)
(**************************************************************************)

(* [c99_scoping] is [true] if using C99/C11 scoping rules, [false] if
   using ANSI scoping rules. *)
(* Should be set to [true] for C99/C11 parsing, [false] for C89/C90
   parsing. *)
let c99_scoping = ref false

(* [atomic_strict_syntax] is [true] if [_Atomic] followed by an
   opening parenthesis is always a type specifier. *)
(* Should be set to [true] for strictly compliant C parsing, but
   setting it to [false] only accepts more programs (and no program can
   be wrongly parsed because of that). *)
let atomic_strict_syntax = ref true
