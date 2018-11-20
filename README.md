C11parser
=========

##### A correct C89/C90/C99/C11/C18 parser written using Menhir and OCaml

The operation and design of this parser are described in detail in the
following journal paper (in process of review for publication):

> A simple, possibly correct LR parser for C11<br/>
> Jacques-Henri Jourdan and François Pottier<br/>
> 2017<br/>

How to use the parser?
----------------------

You need to have installed
- [OCaml](https://ocaml.org/docs/install.html) (known to work with 4.05.0),
- the [Menhir](http://gallium.inria.fr/~fpottier/menhir/) parser generator
  (known to work with 20181113) and
- [ocamlbuild](https://github.com/ocaml/ocamlbuild) (known to work with 0.12.0)

In order to build it, you can just type `make`.

The executable that is produced takes a preprocessed C file in its
standard input and raises an exception in the case of a parse
error.

The following command-line options are available:
  - `-std {c89|c90|c99|c11|c18}`

    Sets which grammar to use.

    `c89` and `c90` tells the parser to use the old grammar, where
    declaration were not required to have a type specifier, in which
    case "int" was used (it still recognizes C99, C11 and C18 constructs).

    `c99`, `c11` and `c18` use the new, simpler grammar: Declarations are
    required to have a type specifier, and the scoping rules are
    different.

  - `-c99-scoping`

    Use the C99/C11/C18 scoping rules even though the old C89/C90 grammar
    is used. This is always set when using the new grammar.

  - `-atomic-permissive-syntax`

    The C18 standard forbids the use of an opening parenthesis
    immediately following an atomic type qualifier. This is intended
    to avoid a possible ambiguity with _Atomic used in a type
    specifier. This parser disambiguates this apparent conflict so
    that this restriction can be lifted safely.

If you want to use this parser in a C front-end, you should fill the
semantic actions of .mly files with your own code for building your
AST:
  - The file `parser.mly` contains a C99/C11/C18 compliant parser. It
    mostly follows the grammar of the C18 standard.
  - The file `parser_ansi_compatible.mly` is compliant with C89, C99,
    C11 and C18 (depending on the options given in `options.ml`). It
    is significantly more complex than `parser.mly`.

The test suite
--------------

We provide, in the `tests/` directory, a series of tests that are
particularly difficult to handle in a correct C parser. They are all
valid C18 fragments, except for:
  - The files whose name end with `.fail.c`
  - `atomic_parenthesis.c`, which represents an unnecessary restriction
    in the syntax presented in the C18 standard.

In order to run the test suite, you need the
[cram](https://bitheap.org/cram/) tool, available on most major linux
distributions. Then, simply type `make test`.
