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

(* WARNING. When processing this grammar, Menhir should announce that
   ONE shift/reduce conflict was silently solved and that ONE state
   has 3 reduce/reduce conflicts on RPAREN, LPAREN, and LBRACK. If you
   modify the grammar, you should check that this is still the case. *)

%{
  open Context
  open Declarator
%}

%token<string> NAME
%token VARIABLE TYPE
%token CONSTANT STRING_LITERAL

%token ALIGNAS "_Alignas"
%token ALIGNOF "_Alignof"
%token ATOMIC "_Atomic"
%token AUTO "auto"
%token BOOL "_Bool"
%token BREAK "break"
%token CASE "case"
%token CHAR "char"
%token COMPLEX "_Complex"
%token CONST "const"
%token CONTINUE "continue"
%token DEFAULT "default"
%token DO "do"
%token DOUBLE "double"
%token ELSE "else"
%token ENUM "enum"
%token EXTERN "extern"
%token FLOAT "float"
%token FOR "for"
%token GENERIC "_Generic"
%token GOTO "goto"
%token IF "if"
%token IMAGINARY "_Imaginary"
%token INLINE "inline"
%token INT "int"
%token LONG "long"
%token NORETURN "_Noreturn"
%token REGISTER "register"
%token RESTRICT "restrict"
%token RETURN "return"
%token SHORT "short"
%token SIGNED "signed"
%token SIZEOF "sizeof"
%token STATIC "static"
%token STATIC_ASSERT "_Static_assert"
%token STRUCT "struct"
%token SWITCH "switch"
%token THREAD_LOCAL "_Thread_local"
%token TYPEDEF "typedef"
%token UNION "union"
%token UNSIGNED "unsigned"
%token VOID "void"
%token VOLATILE "volatile"
%token WHILE "while"

%token ELLIPSIS "..."
%token ADD_ASSIGN "+="
%token SUB_ASSIGN "-="
%token MUL_ASSIGN "*="
%token DIV_ASSIGN "/="
%token MOD_ASSIGN "%="
%token OR_ASSIGN "|="
%token AND_ASSIGN "&="
%token XOR_ASSIGN "^="
%token LEFT_ASSIGN "<<="
%token RIGHT_ASSIGN ">>="
%token LEFT "<<"
%token RIGHT ">>"
%token EQEQ "=="
%token NEQ "!="
%token LEQ "<="
%token GEQ ">="
%token EQ "="
%token LT "<"
%token GT ">"
%token INC "++"
%token DEC "--"
%token PTR "->"
%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token SLASH "/"
%token PERCENT "%"
%token BANG "!"
%token ANDAND "&&"
%token BARBAR "||"
%token AND "&"
%token BAR "|"
%token HAT "^"
%token QUESTION "?"
%token COLON ":"
%token TILDE "~"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACK "["
%token RBRACK "]"
%token LPAREN "("
%token RPAREN ")"
%token SEMICOLON ";"
%token COMMA ","
%token DOT "."

(* ATOMIC_LPAREN is "special"; it's used for left parentheses that
   follow the ["_Atomic"] keyword. It isn't given a token alias *)
%token ATOMIC_LPAREN

%token EOF

%type<context> save_context parameter_type_list function_definition1
%type<string> typedef_name var_name general_identifier enumeration_constant
%type<declarator> declarator direct_declarator

(* There is a reduce/reduce conflict in the grammar. It corresponds to the
   conflict in the second declaration in the following snippet:

     typedef int T;
     int f(int(T));

   It is specified by 6.7.6.3 11: 'T' should be taken as the type of the
   parameter of the anonymous function taken as a parameter by f (thus,
   f has type (T -> int) -> int).

   The reduce/reduce conflict is solved by letting menhir reduce the production
   appearing first in this file. This is the reason why we have the
   [typedef_name_spec] proxy: it is here just to make sure the conflicting
   production appears before the other (which concerns [general_identifier]). *)

(* These precedence declarations solve the dangling else conflict. *)
%nonassoc below_ELSE
%nonassoc ELSE

%start<unit> translation_unit_file

%%

(* Helpers *)

(* [option(X)] represents a choice between nothing and [X].
   [ioption(X)] is the same thing, but is inlined at its use site,
   which in some cases is necessary in order to avoid a conflict.
   By convention, [X?] is syntactic sugar for [option(X)]. *)

%inline ioption(X):
| /* nothing */
| X
    {}

option(X):
| o = ioption(X)
    { o }

(* By convention, [X*] is syntactic sugar for [list(X)]. *)

list(X):
| /* nothing */
| X list(X)
    {}

(* A list of A's and B's that contains exactly one A: *)

list_eq1(A, B):
| A B*
| B list_eq1(A, B)
    {}

(* A list of A's and B's that contains at least one A: *)

list_ge1(A, B):
| A B*
| A list_ge1(A, B)
| B list_ge1(A, B)
    {}

(* A list of A's, B's and C's that contains exactly one A and exactly one B: *)

list_eq1_eq1(A, B, C):
| A list_eq1(B, C)
| B list_eq1(A, C)
| C list_eq1_eq1(A, B, C)
    {}

(* A list of A's, B's and C's that contains exactly one A and at least one B: *)

list_eq1_ge1(A, B, C):
| A list_ge1(B, C)
| B list_eq1(A, C)
| B list_eq1_ge1(A, B, C)
| C list_eq1_ge1(A, B, C)
    {}

(* Upon finding an identifier, the lexer emits two tokens. The first token,
   [NAME], indicates that a name has been found; the second token, either [TYPE]
   or [VARIABLE], tells what kind of name this is. The classification is
   performed only when the second token is demanded by the parser. *)

typedef_name:
| i = NAME TYPE
    { i }

var_name:
| i = NAME VARIABLE
    { i }

(* [typedef_name_spec] must be declared before [general_identifier], so that the
   reduce/reduce conflict is solved the right way. *)

typedef_name_spec:
| typedef_name
    {}

general_identifier:
| i = typedef_name
| i = var_name
    { i }

save_context:
| (* empty *)
    { save_context () }

scoped(X):
| ctx = save_context x = X
    { restore_context ctx; x }

(* [declarator_varname] and [declarator_typedefname] are like [declarator]. In
   addition, they have the side effect of introducing the declared identifier as
   a new variable or typedef name in the current context. *)

declarator_varname:
| d = declarator
    { declare_varname (identifier d); d }

declarator_typedefname:
| d = declarator
    { declare_typedefname (identifier d); d }

(* Merge source-level string literals. *)
string_literal:
| STRING_LITERAL
| string_literal STRING_LITERAL
    {}

(* End of the helpers, and beginning of the grammar proper: *)

primary_expression:
| var_name
| CONSTANT
| string_literal
| "(" expression ")"
| generic_selection
    {}

generic_selection:
| "_Generic" "(" assignment_expression "," generic_assoc_list ")"
    {}

generic_assoc_list:
| generic_association
| generic_assoc_list "," generic_association
    {}

generic_association:
| type_name ":" assignment_expression
| "default" ":" assignment_expression
    {}

postfix_expression:
| primary_expression
| postfix_expression "[" expression "]"
| postfix_expression "(" argument_expression_list? ")"
| postfix_expression "." general_identifier
| postfix_expression "->" general_identifier
| postfix_expression "++"
| postfix_expression "--"
| "(" type_name ")" "{" initializer_list ","? "}"
    {}

argument_expression_list:
| assignment_expression
| argument_expression_list "," assignment_expression
    {}

unary_expression:
| postfix_expression
| "++" unary_expression
| "--" unary_expression
| unary_operator cast_expression
| "sizeof" unary_expression
| "sizeof" "(" type_name ")"
| "_Alignof" "(" type_name ")"
    {}

unary_operator:
| "&"
| "*"
| "+"
| "-"
| "~"
| "!"
    {}

cast_expression:
| unary_expression
| "(" type_name ")" cast_expression
    {}

multiplicative_operator:
  "*" | "/" | "%" {}

multiplicative_expression:
| cast_expression
| multiplicative_expression multiplicative_operator cast_expression
    {}

additive_operator:
  "+" | "-" {}

additive_expression:
| multiplicative_expression
| additive_expression additive_operator multiplicative_expression
    {}

shift_operator:
  "<<" | ">>" {}

shift_expression:
| additive_expression
| shift_expression shift_operator additive_expression
    {}

relational_operator:
  "<" | ">" | "<=" | ">=" {}

relational_expression:
| shift_expression
| relational_expression relational_operator shift_expression
    {}

equality_operator:
  "==" | "!=" {}

equality_expression:
| relational_expression
| equality_expression equality_operator relational_expression
    {}

and_expression:
| equality_expression
| and_expression "&" equality_expression
    {}

exclusive_or_expression:
| and_expression
| exclusive_or_expression "^" and_expression
    {}

inclusive_or_expression:
| exclusive_or_expression
| inclusive_or_expression "|" exclusive_or_expression
    {}

logical_and_expression:
| inclusive_or_expression
| logical_and_expression "&&" inclusive_or_expression
    {}

logical_or_expression:
| logical_and_expression
| logical_or_expression "||" logical_and_expression
    {}

conditional_expression:
| logical_or_expression
| logical_or_expression "?" expression ":" conditional_expression
    {}

assignment_expression:
| conditional_expression
| unary_expression assignment_operator assignment_expression
    {}

assignment_operator:
| "="
| "*="
| "/="
| "%="
| "+="
| "-="
| "<<="
| ">>="
| "&="
| "^="
| "|="
    {}

expression:
| assignment_expression
| expression "," assignment_expression
    {}


constant_expression:
| conditional_expression
    {}

(* We separate type declarations, which contain an occurrence of ["typedef"], and
   normal declarations, which do not. This makes it possible to distinguish /in
   the grammar/ whether a declaration introduces typedef names or variables in
   the context. *)

declaration:
| declaration_specifiers         init_declarator_list(declarator_varname)?     ";"
| declaration_specifiers_typedef init_declarator_list(declarator_typedefname)? ";"
| static_assert_declaration
    {}

(* [declaration_specifier] corresponds to one declaration specifier in the C18
   standard, deprived of "typedef" and of type specifiers. *)

declaration_specifier:
| storage_class_specifier (* deprived of "typedef" *)
| type_qualifier
| function_specifier
| alignment_specifier
    {}

(* [declaration_specifiers] requires that at least one type specifier be
   present, and, if a unique type specifier is present, then no other type
   specifier be present. In other words, one should have either at least one
   nonunique type specifier, or exactly one unique type specifier.

   This is a weaker condition than 6.7.2 2. Encoding this condition in the
   grammar is necessary to disambiguate the example in 6.7.7 6:

     typedef signed int t;
     struct tag {
     unsigned t:4;
     const t:5;
     };

   The first field is a named t, while the second is unnamed of type t.

   [declaration_specifiers] forbids the ["typedef"] keyword. *)

declaration_specifiers:
| list_eq1(type_specifier_unique,    declaration_specifier)
| list_ge1(type_specifier_nonunique, declaration_specifier)
    {}

(* [declaration_specifiers_typedef] is analogous to [declaration_specifiers],
   but requires the ["typedef"] keyword to be present (exactly once). *)

declaration_specifiers_typedef:
| list_eq1_eq1("typedef", type_specifier_unique,    declaration_specifier)
| list_eq1_ge1("typedef", type_specifier_nonunique, declaration_specifier)
    {}

(* The parameter [declarator] in [init_declarator_list] and [init_declarator]
   is instantiated with [declarator_varname] or [declarator_typedefname]. *)

init_declarator_list(declarator):
| init_declarator(declarator)
| init_declarator_list(declarator) "," init_declarator(declarator)
    {}

init_declarator(declarator):
| declarator
| declarator "=" c_initializer
    {}

(* [storage_class_specifier] corresponds to storage-class-specifier in the
   C18 standard, deprived of ["typedef"] (which receives special treatment). *)

storage_class_specifier:
| "extern"
| "static"
| "_Thread_local"
| "auto"
| "register"
    {}

(* A type specifier which can appear together with other type specifiers. *)

type_specifier_nonunique:
| "char"
| "short"
| "int"
| "long"
| "float"
| "double"
| "signed"
| "unsigned"
| "_Complex"
    {}

(* A type specifier which cannot appear together with other type specifiers. *)

type_specifier_unique:
| "void"
| "_Bool"
| atomic_type_specifier
| struct_or_union_specifier
| enum_specifier
| typedef_name_spec
    {}

struct_or_union_specifier:
| struct_or_union general_identifier? "{" struct_declaration_list "}"
| struct_or_union general_identifier
    {}

struct_or_union:
| "struct"
| "union"
    {}

struct_declaration_list:
| struct_declaration
| struct_declaration_list struct_declaration
    {}

struct_declaration:
| specifier_qualifier_list struct_declarator_list? ";"
| static_assert_declaration
    {}


(* [specifier_qualifier_list] is as in the standard, except it also encodes the
   same constraint as [declaration_specifiers] (see above). *)

specifier_qualifier_list:
| list_eq1(type_specifier_unique,    type_qualifier | alignment_specifier {})
| list_ge1(type_specifier_nonunique, type_qualifier | alignment_specifier {})
    {}

struct_declarator_list:
| struct_declarator
| struct_declarator_list "," struct_declarator
    {}

struct_declarator:
| declarator
| declarator? ":" constant_expression
    {}

enum_specifier:
| "enum" general_identifier? "{" enumerator_list ","? "}"
| "enum" general_identifier
    {}

enumerator_list:
| enumerator
| enumerator_list "," enumerator
    {}

enumerator:
| i = enumeration_constant
| i = enumeration_constant "=" constant_expression
    { declare_varname i }

enumeration_constant:
| i = general_identifier
    { i }

atomic_type_specifier:
| "_Atomic" "(" type_name ")"
| "_Atomic" ATOMIC_LPAREN type_name ")"
    {}

type_qualifier:
| "const"
| "restrict"
| "volatile"
| "_Atomic"
    {}

function_specifier:
  "inline" | "_Noreturn"
    {}

alignment_specifier:
| "_Alignas" "(" type_name ")"
| "_Alignas" "(" constant_expression ")"
    {}

declarator:
| ioption(pointer) d = direct_declarator
    { other_declarator d }

(* The occurrences of [save_context] inside [direct_declarator] and
   [direct_abstract_declarator] seem to serve no purpose. In fact, they are
   required in order to avoid certain conflicts. In other words, we must save
   the context at this point because the LR automaton is exploring multiple
   avenues in parallel and some of them do require saving the context. *)

direct_declarator:
| i = general_identifier
    { identifier_declarator i }
| "(" save_context d = declarator ")"
    { d }
| d = direct_declarator "[" type_qualifier_list? assignment_expression? "]"
| d = direct_declarator "[" "static" type_qualifier_list? assignment_expression "]"
| d = direct_declarator "[" type_qualifier_list "static" assignment_expression "]"
| d = direct_declarator "[" type_qualifier_list? "*" "]"
    { other_declarator d }
| d = direct_declarator "(" ctx = scoped(parameter_type_list) ")"
    { function_declarator d ctx }
| d = direct_declarator "(" save_context identifier_list? ")"
    { other_declarator d }

pointer:
| "*" type_qualifier_list? pointer?
    {}

type_qualifier_list:
| type_qualifier_list? type_qualifier
    {}

parameter_type_list:
| parameter_list option("," "..." {}) ctx = save_context
    { ctx }

parameter_list:
| parameter_declaration
| parameter_list "," parameter_declaration
    {}

parameter_declaration:
| declaration_specifiers declarator_varname
| declaration_specifiers abstract_declarator?
    {}

identifier_list:
| var_name
| identifier_list "," var_name
    {}

type_name:
| specifier_qualifier_list abstract_declarator?
    {}

abstract_declarator:
| pointer
| ioption(pointer) direct_abstract_declarator
    {}

direct_abstract_declarator:
| "(" save_context abstract_declarator ")"
| direct_abstract_declarator? "[" ioption(type_qualifier_list) assignment_expression? "]"
| direct_abstract_declarator? "[" "static" type_qualifier_list? assignment_expression "]"
| direct_abstract_declarator? "[" type_qualifier_list "static" assignment_expression "]"
| direct_abstract_declarator? "[" "*" "]"
| ioption(direct_abstract_declarator) "(" scoped(parameter_type_list)? ")"
    {}

c_initializer:
| assignment_expression
| "{" initializer_list ","? "}"
    {}

initializer_list:
| designation? c_initializer
| initializer_list "," designation? c_initializer
    {}

designation:
| designator_list "="
    {}

designator_list:
| designator_list? designator
    {}

designator:
| "[" constant_expression "]"
| "." general_identifier
    {}

static_assert_declaration:
| "_Static_assert" "(" constant_expression "," string_literal ")" ";"
    {}

statement:
| labeled_statement
| scoped(compound_statement)
| expression_statement
| scoped(selection_statement)
| scoped(iteration_statement)
| jump_statement
    {}

labeled_statement:
| general_identifier ":" statement
| "case" constant_expression ":" statement
| "default" ":" statement
    {}

compound_statement:
| "{" block_item_list? "}"
    {}

block_item_list:
| block_item_list? block_item
    {}

block_item:
| declaration
| statement
    {}

expression_statement:
| expression? ";"
    {}

selection_statement:
| "if" "(" expression ")" scoped(statement) "else" scoped(statement)
| "if" "(" expression ")" scoped(statement) %prec below_ELSE
| "switch" "(" expression ")" scoped(statement)
    {}

iteration_statement:
| "while" "(" expression ")" scoped(statement)
| "do" scoped(statement) "while" "(" expression ")" ";"
| "for" "(" expression? ";" expression? ";" expression? ")" scoped(statement)
| "for" "(" declaration expression? ";" expression? ")" scoped(statement)
    {}

jump_statement:
| "goto" general_identifier ";"
| "continue" ";"
| "break" ";"
| "return" expression? ";"
    {}

translation_unit_file:
| external_declaration translation_unit_file
| external_declaration EOF
    {}

external_declaration:
| function_definition
| declaration
    {}

function_definition1:
| declaration_specifiers d = declarator_varname
    { let ctx = save_context () in
      reinstall_function_context d;
      ctx }

function_definition:
| ctx = function_definition1 declaration_list? compound_statement
    { restore_context ctx }

declaration_list:
| declaration
| declaration_list declaration
    {}
