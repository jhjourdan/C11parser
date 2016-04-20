open Context

(* We distinguish between three kinds of declarators: 1- identifiers,
   2- function declarators, and 3- everything else. In the case of a
   function declarator, we save a snapshot of the context at the END
   of the parameter-type-list. *)

(* With a declarator, we associate two pieces of information: 1- the
   identifier that is being declared; 2- the declarator's kind, as
   defined above. *)

type declarator

(* This accessor returns the identifier that is being declared. *)

val identifier: declarator -> string

(* Three functions for constructing declarators. *)

val identifier_declarator: string -> declarator
val function_declarator: declarator -> context -> declarator
val other_declarator: declarator -> declarator

(* A function for restoring the context that was saved in a function
   declarator and, on top of that, declaring the function itself as a
   variable. *)

val reinstall_function_context: declarator -> unit

