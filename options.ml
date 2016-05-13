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
(* This flag influences only the ANSI-compatible parser. It should be
   set to [true] for C99/C11 parsing, [false] for C89/C90 parsing. *)
let c99_scoping = ref false

(* [atomic_strict_syntax] is [true] if [_Atomic] followed by an
   opening parenthesis is always a type specifier. *)
(* It should be set to [true] for strictly compliant C11 parsing.
   Setting it to [false] only causes more programs to be accepted; no
   program can be parsed incorrectly. *)
(* This flag directly influences only the lexer. *)
let atomic_strict_syntax = ref true
