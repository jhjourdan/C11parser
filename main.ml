open Lexer
open Options

let _ =
  let lexbuf = Lexing.from_channel stdin in
  if allow_implicit_int then
    Parser_ansi_compatible.translation_unit_file lexer lexbuf
  else
    Parser.translation_unit_file lexer lexbuf
