open Ocamlbuild_plugin
open Command

let () =
  dispatch (function After_rules ->
    (* Nazi warnings *)
    flag ["ocaml"; "compile"] (S[A "-w"; A "@1..49-4-9-41-44"])
  | _ -> ()
  )
