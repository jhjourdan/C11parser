C11parser
=========

##### A correct C11 parser written using Menhir and OCaml

The operation and design of this parser are described in detail in the
following journal paper (in process of review for publication):

> A correct LR parser for C11
> Jacques-Henri Jourdan and François Pottier
> 2016

How to use the parser?
----------------------

You need to have installed OCaml, the Menhir parser generator and
ocamlbuild. In order to build it, you can just type `make`.

The executable that is produced takes a preprocessed C file in its
standard input and raises an exception in the case of a parse
error. Some configuration options are provided in the `option.ml`
file. In particular, the parser is configurable to run in C89
compliant mode (which is incompatible with the C99/C11 mode).

If you want to use this parser in a C front-end, you should fill the
semantic actions of .mly files with your own code for building your
AST:
  - The file `parser.mly` contains a C99/C11 compliant parser. It
    mostly follows the grammar of the C11 standard.
  - The file `parser_ansi_compatible.mly` is compliant with C89, C99 and
    C11 (depending on the options given in options.ml). It is
    significantly more complex than `parser.mly`.

The test suite
--------------

We provide, in the tests directory, a series of tests that are
particularly difficult to handle in a correct C parser. They are all
valid C11 fragments, except for:
  - The files whose name finishes with .fail.c
  - `atomic_parenthesis.c`, which represent an unecessary restriction in
    the syntax presented in the C11 standard.
