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


let parser = ref (fun _ _ -> assert false)

let set_std = function
  | "c89" | "c90" ->
    parser := Parser_ansi_compatible.translation_unit_file
  | "c99" | "c11" ->
    parser := Parser.translation_unit_file
  | _ -> assert false

let usage_msg =
"\nThis is a C89/C90/C99/C11 compliant parser written in OCaml. It reads\n\
a preprocessed C file in standard input and raises an exception if it\n\
contains invalid syntax.\n\
Options available:"

let opts = [
    "-std",                      Arg.Symbol (["c89"; "c90"; "c99"; "c11"], set_std),
    " Sets which grammar to use.";
    "-c99-scoping",              Arg.Set Options.c99_scoping,
    " When using the c89/c90 grammar, uses C99 scoping rules instead of the C89 ones. ";
    "-atomic-permissive-syntax", Arg.Clear Options.atomic_strict_syntax,
    " An opening parenthesis after an _Atomic type qualifier is not a syntax error.";
  ]
let opts = Arg.align ?limit:(Some 1000) opts

let () = parser := fun _ _ ->
  Printf.eprintf "No -std option specified.\n";
  Arg.usage opts usage_msg;
  exit 1

let _ =
  Arg.parse opts
            (fun o -> raise (Arg.Bad (Printf.sprintf "Unrecognized option \"%s\"" o)))
            usage_msg;
  let lexbuf = Lexing.from_channel stdin in
  !parser lexer lexbuf
