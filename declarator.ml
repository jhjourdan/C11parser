(* **********************************************************************)
(*                                                                      *)
(*          Jacques-Henri Jourdan, INRIA Paris                          *)
(*          François Pottier, INRIA Paris                               *)
(*                                                                      *)
(*  Copyright Institut National de Recherche en Informatique et en      *)
(*  Automatique.  All rights reserved.  This file is distributed        *)
(*  under the terms of the GNU General Public License as published by   *)
(*  the Free Software Foundation, either version 2 of the License, or   *)
(*  (at your option) any later version.  This file is also distributed  *)
(*  under the terms of the INRIA Non-Commercial License Agreement.      *)
(*                                                                      *)
(* **********************************************************************)

open Context

(* We distinguish between three kinds of declarators: 1- identifiers,
   2- function declarators, and 3- everything else. In the case of a
   function declarator, we save a snapshot of the context at the END
   of the parameter-type-list. *)

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
      (* This should not happen. It would mean that we have encountered
         a declarator that is not a function declarator yet is followed
         by [declaration_list? compound_statement]. An error can be
         reported if this happens. *)
      ()
