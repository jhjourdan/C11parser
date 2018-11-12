(*
Jacques-Henri Jourdan, Inria Paris
François Pottier, Inria Paris

Copyright (c) 2016-2017, Inria
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Inria nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL INRIA BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

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
