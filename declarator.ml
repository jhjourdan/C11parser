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

open Context

(* We distinguish between three kinds of declarators: 1- identifiers,
   2- function declarators, and 3- everything else. In the case of a
   function declarator, we save a snapshot of the context at the END
   of the parameter-type-list. *)

(* K&R function declarators are considered part of "other" declarators. *)

type declarator_kind =
| DeclaratorIdentifier
| DeclaratorFunction of context
| DeclaratorOther

(* With a declarator, we associate two pieces of information: 1- the
   identifier that is being declared; 2- the declarator's kind, as
   defined above. *)

type declarator = {
  identifier: string;
  kind: declarator_kind
}

(* This accessor returns the identifier that is being declared. *)

let identifier d =
  d.identifier

(* Three functions for constructing declarators. *)

let identifier_declarator i =
  { identifier = i; kind = DeclaratorIdentifier }

let function_declarator d ctx =
  match d.kind with
  | DeclaratorIdentifier -> { d with kind = DeclaratorFunction ctx }
  | _                    ->   d

let other_declarator d =
  match d.kind with
  | DeclaratorIdentifier -> { d with kind = DeclaratorOther }
  | _                    ->   d

(* A function for restoring the context that was saved in a function
   declarator and, on top of that, declaring the function itself as a
   variable. *)

let reinstall_function_context d =
  match d.kind with
  | DeclaratorFunction ctx ->
      restore_context ctx;
      declare_varname d.identifier
  | _ ->
      (* If we are here, then we have encountered a declarator that is
         not a function declarator yet is followed by (the first symbol
         of) [declaration_list? compound_statement]. Either this is a
         K&R function declarator (in which case we should do nothing)
         or this is an error. *)
      ()
