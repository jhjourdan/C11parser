C11parser
=========

### A correct C11 parser written using Menhir and OCaml

The operation and design of this parser are described in detail in the
following journal paper (in process of review for publication):

<quote>
  A correct LR parser for C11
  Jacques-Henri Jourdan and François Pottier
  2016
</quote>

How to use it ?
---------------

You need to have installed OCaml, the Menhir parser generator and
ocamlbuild. To build it, you can just type 


. If used in a C front-end,
you should fill the semantic actions of .mly files with your own AST
building code.

See options.ml for the possible configuration options.
