(**************************************************************************)
(*                    Jacques-Henri Jourdan, Inria Paris                  *)
(*                      François Pottier, Inria Paris                     *)
(*                                                                        *)
(*  Copyright Inria. All rights reserved. This file is distributed under  *)
(*  the terms of the GNU General Public License as published by the Free  *)
(*  Software Foundation, either version 2 of the License, or (at your     *)
(*  option) any later version.                                            *)
(**************************************************************************)

open Lexer
open Options

let _ =
  let lexbuf = Lexing.from_channel stdin in
  if allow_implicit_int then
    Parser_ansi_compatible.translation_unit_file lexer lexbuf
  else
    Parser.translation_unit_file lexer lexbuf
