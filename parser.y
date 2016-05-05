/**************************************************************************/
/*                    Jacques-Henri Jourdan, Inria Paris                  */
/*                      François Pottier, Inria Paris                     */
/*                                                                        */
/*  Copyright Inria. All rights reserved. This file is distributed under  */
/*  the terms of the GNU General Public License as published by the Free  */
/*  Software Foundation, either version 2 of the License, or (at your     */
/*  option) any later version.                                            */
/**************************************************************************/

%start translation_unit_file
%token XOR_ASSIGN
%token WHILE
%token VOLATILE
%token VOID
%token VARIABLE
%token UNSIGNED
%token UNION
%token TYPEDEF
%token TYPE
%token TILDE
%token THREAD_LOCAL
%token SWITCH
%token SUB_ASSIGN
%token STRUCT
%token STRING_LITERAL
%token STATIC_ASSERT
%token STATIC
%token STAR
%token SLASH
%token SIZEOF
%token SIGNED
%token SHORT
%token SEMICOLON
%token RPAREN
%token RIGHT_ASSIGN
%token RIGHT
%token RETURN
%token RESTRICT
%token REGISTER
%token RBRACK
%token RBRACE
%token QUESTION
%token PTR
%token PLUS
%token PERCENT
%token OR_ASSIGN
%token NORETURN
%token NEQ
%token NAME
%token MUL_ASSIGN
%token MOD_ASSIGN
%token MINUS
%token LT
%token LPAREN
%token LONG
%token LEQ
%token LEFT_ASSIGN
%token LEFT
%token LBRACK
%token LBRACE
%token INT
%token INLINE
%token INC
%token IMAGINARY
%token IF
%token HAT
%token GT
%token GOTO
%token GEQ
%token GENERIC
%token FOR
%token FLOAT
%token EXTERN
%token EQEQ
%token EQ
%token EOF
%token ENUM
%token ELLIPSIS
%token DOUBLE
%token DOT
%token DO
%token DIV_ASSIGN
%token DEFAULT
%token DEC
%token CONTINUE
%token CONSTANT
%token CONST
%token COMPLEX
%token COMMA
%token COLON
%token CHAR
%token CASE
%token BREAK
%token BOOL
%token BARBAR
%token BAR
%token BANG
%token AUTO
%token ATOMIC_LPAREN
%token ATOMIC
%token AND_ASSIGN
%token ANDAND
%token AND
%token ALIGNOF
%token ALIGNAS
%token ADD_ASSIGN
%token ELSE
%nonassoc below_ELSE 
%nonassoc ELSE 
%type <unit> declarator
%type <unit> direct_declarator
%type <unit> enumeration_constant
%type <unit> function_definition1
%type <unit> general_identifier
%type <unit> parameter_type_list
%type <unit> save_context
%type <unit> translation_unit_file
%type <unit> typedef_name
%type <unit> var_name
%%

option_type_qualifier_list_:
  
    {}
| type_qualifier_list
    {}

option_struct_declarator_list_:
  
    {}
| struct_declarator_list
    {}

option_scoped_parameter_type_list__:
  
    {}
| scoped_parameter_type_list_
    {}

option_pointer_:
  
    {}
| pointer
    {}

option_init_declarator_list_declarator_varname__:
  
    {}
| init_declarator_list_declarator_varname_
    {}

option_init_declarator_list_declarator_typedefname__:
  
    {}
| init_declarator_list_declarator_typedefname_
    {}

option_identifier_list_:
  
    {}
| identifier_list
    {}

option_general_identifier_:
  
    {}
| general_identifier
    {}

option_expression_:
  
    {}
| expression
    {}

option_direct_abstract_declarator_:
  
    {}
| direct_abstract_declarator
    {}

option_designator_list_:
  
    {}
| designator_list
    {}

option_designation_:
  
    {}
| designation
    {}

option_declarator_:
  
    {}
| declarator
    {}

option_declaration_list_:
  
    {}
| declaration_list
    {}

option_block_item_list_:
  
    {}
| block_item_list
    {}

option_assignment_expression_:
  
    {}
| assignment_expression
    {}

option_argument_expression_list_:
  
    {}
| argument_expression_list
    {}

option_abstract_declarator_:
  
    {}
| abstract_declarator
    {}

option___anonymous_0_:
  
    {}
| COMMA ELLIPSIS
    {}

option_COMMA_:
  
    {}
| COMMA
    {}

list_type_qualifier_:
  
    {}
| type_qualifier list_type_qualifier_
    {}

list_declaration_specifier_:
  
    {}
| declaration_specifier list_declaration_specifier_
    {}

list_eq1_type_specifier_unique_type_qualifier_:
  type_specifier_unique list_type_qualifier_
    {}
| type_qualifier list_eq1_type_specifier_unique_type_qualifier_
    {}

list_eq1_type_specifier_unique_declaration_specifier_:
  type_specifier_unique list_declaration_specifier_
    {}
| declaration_specifier list_eq1_type_specifier_unique_declaration_specifier_
    {}

list_eq1_TYPEDEF_declaration_specifier_:
  TYPEDEF list_declaration_specifier_
    {}
| declaration_specifier list_eq1_TYPEDEF_declaration_specifier_
    {}

list_ge1_type_specifier_nonunique_type_qualifier_:
  type_specifier_nonunique list_type_qualifier_
    {}
| type_specifier_nonunique list_ge1_type_specifier_nonunique_type_qualifier_
    {}
| type_qualifier list_ge1_type_specifier_nonunique_type_qualifier_
    {}

list_ge1_type_specifier_nonunique_declaration_specifier_:
  type_specifier_nonunique list_declaration_specifier_
    {}
| type_specifier_nonunique list_ge1_type_specifier_nonunique_declaration_specifier_
    {}
| declaration_specifier list_ge1_type_specifier_nonunique_declaration_specifier_
    {}

list_eq1_eq1_TYPEDEF_type_specifier_unique_declaration_specifier_:
  TYPEDEF list_eq1_type_specifier_unique_declaration_specifier_
    {}
| type_specifier_unique list_eq1_TYPEDEF_declaration_specifier_
    {}
| declaration_specifier list_eq1_eq1_TYPEDEF_type_specifier_unique_declaration_specifier_
    {}

list_eq1_ge1_TYPEDEF_type_specifier_nonunique_declaration_specifier_:
  TYPEDEF list_ge1_type_specifier_nonunique_declaration_specifier_
    {}
| type_specifier_nonunique list_eq1_TYPEDEF_declaration_specifier_
    {}
| type_specifier_nonunique list_eq1_ge1_TYPEDEF_type_specifier_nonunique_declaration_specifier_
    {}
| declaration_specifier list_eq1_ge1_TYPEDEF_type_specifier_nonunique_declaration_specifier_
    {}

typedef_name:
  NAME TYPE
    {}

typedef_name_spec:
  typedef_name
    {}

var_name:
  NAME VARIABLE
    {}

general_identifier:
  typedef_name
    {}
| var_name
    {}

save_context:
  
    {}

scoped_parameter_type_list_:
  save_context parameter_type_list
    {}

scoped_compound_statement_:
  save_context compound_statement
    {}

c99_scoped_statement_:
  save_context statement
    {}

c99_scoped_selection_statement_:
  save_context selection_statement
    {}

c99_scoped_iteration_statement_:
  save_context iteration_statement
    {}

declarator_varname:
  declarator
    {}

declarator_typedefname:
  declarator
    {}

primary_expression:
  var_name
    {}
| CONSTANT
    {}
| STRING_LITERAL
    {}
| LPAREN expression RPAREN
    {}
| generic_selection
    {}

generic_selection:
  GENERIC LPAREN assignment_expression COMMA generic_assoc_list RPAREN
    {}

generic_assoc_list:
  generic_association
    {}
| generic_assoc_list COMMA generic_association
    {}

generic_association:
  type_name COLON assignment_expression
    {}
| DEFAULT COLON assignment_expression
    {}

postfix_expression:
  primary_expression
    {}
| postfix_expression LBRACK expression RBRACK
    {}
| postfix_expression LPAREN option_argument_expression_list_ RPAREN
    {}
| postfix_expression DOT general_identifier
    {}
| postfix_expression PTR general_identifier
    {}
| postfix_expression INC
    {}
| postfix_expression DEC
    {}
| LPAREN type_name RPAREN LBRACE initializer_list option_COMMA_ RBRACE
    {}

argument_expression_list:
  assignment_expression
    {}
| argument_expression_list COMMA assignment_expression
    {}

unary_expression:
  postfix_expression
    {}
| INC unary_expression
    {}
| DEC unary_expression
    {}
| unary_operator cast_expression
    {}
| SIZEOF unary_expression
    {}
| SIZEOF LPAREN type_name RPAREN
    {}
| ALIGNOF LPAREN type_name RPAREN
    {}

unary_operator:
  AND
    {}
| STAR
    {}
| PLUS
    {}
| MINUS
    {}
| TILDE
    {}
| BANG
    {}

cast_expression:
  unary_expression
    {}
| LPAREN type_name RPAREN cast_expression
    {}

multiplicative_operator:
  STAR
    {}
| SLASH
    {}
| PERCENT
    {}

multiplicative_expression:
  cast_expression
    {}
| multiplicative_expression multiplicative_operator cast_expression
    {}

additive_operator:
  PLUS
    {}
| MINUS
    {}

additive_expression:
  multiplicative_expression
    {}
| additive_expression additive_operator multiplicative_expression
    {}

shift_operator:
  LEFT
    {}
| RIGHT
    {}

shift_expression:
  additive_expression
    {}
| shift_expression shift_operator additive_expression
    {}

relational_operator:
  LT
    {}
| GT
    {}
| LEQ
    {}
| GEQ
    {}

relational_expression:
  shift_expression
    {}
| relational_expression relational_operator shift_expression
    {}

equality_operator:
  EQEQ
    {}
| NEQ
    {}

equality_expression:
  relational_expression
    {}
| equality_expression equality_operator relational_expression
    {}

and_expression:
  equality_expression
    {}
| and_expression AND equality_expression
    {}

exclusive_or_expression:
  and_expression
    {}
| exclusive_or_expression HAT and_expression
    {}

inclusive_or_expression:
  exclusive_or_expression
    {}
| inclusive_or_expression BAR exclusive_or_expression
    {}

logical_and_expression:
  inclusive_or_expression
    {}
| logical_and_expression ANDAND inclusive_or_expression
    {}

logical_or_expression:
  logical_and_expression
    {}
| logical_or_expression BARBAR logical_and_expression
    {}

conditional_expression:
  logical_or_expression
    {}
| logical_or_expression QUESTION expression COLON conditional_expression
    {}

assignment_expression:
  conditional_expression
    {}
| unary_expression assignment_operator assignment_expression
    {}

assignment_operator:
  EQ
    {}
| MUL_ASSIGN
    {}
| DIV_ASSIGN
    {}
| MOD_ASSIGN
    {}
| ADD_ASSIGN
    {}
| SUB_ASSIGN
    {}
| LEFT_ASSIGN
    {}
| RIGHT_ASSIGN
    {}
| AND_ASSIGN
    {}
| XOR_ASSIGN
    {}
| OR_ASSIGN
    {}

expression:
  assignment_expression
    {}
| expression COMMA assignment_expression
    {}

constant_expression:
  conditional_expression
    {}

declaration:
  declaration_specifiers option_init_declarator_list_declarator_varname__ SEMICOLON
    {}
| declaration_specifiers_typedef option_init_declarator_list_declarator_typedefname__ SEMICOLON
    {}
| static_assert_declaration
    {}

declaration_specifier:
  storage_class_specifier
    {}
| type_qualifier
    {}
| function_specifier
    {}
| alignment_specifier
    {}

declaration_specifiers:
  list_eq1_type_specifier_unique_declaration_specifier_
    {}
| list_ge1_type_specifier_nonunique_declaration_specifier_
    {}

declaration_specifiers_typedef:
  list_eq1_eq1_TYPEDEF_type_specifier_unique_declaration_specifier_
    {}
| list_eq1_ge1_TYPEDEF_type_specifier_nonunique_declaration_specifier_
    {}

init_declarator_list_declarator_varname_:
  init_declarator_declarator_varname_
    {}
| init_declarator_list_declarator_varname_ COMMA init_declarator_declarator_varname_
    {}

init_declarator_list_declarator_typedefname_:
  init_declarator_declarator_typedefname_
    {}
| init_declarator_list_declarator_typedefname_ COMMA init_declarator_declarator_typedefname_
    {}

init_declarator_declarator_varname_:
  declarator_varname
    {}
| declarator_varname EQ c_initializer
    {}

init_declarator_declarator_typedefname_:
  declarator_typedefname
    {}
| declarator_typedefname EQ c_initializer
    {}

storage_class_specifier:
  EXTERN
    {}
| STATIC
    {}
| THREAD_LOCAL
    {}
| AUTO
    {}
| REGISTER
    {}

type_specifier_nonunique:
  CHAR
    {}
| SHORT
    {}
| INT
    {}
| LONG
    {}
| FLOAT
    {}
| DOUBLE
    {}
| SIGNED
    {}
| UNSIGNED
    {}
| COMPLEX
    {}

type_specifier_unique:
  VOID
    {}
| BOOL
    {}
| atomic_type_specifier
    {}
| struct_or_union_specifier
    {}
| enum_specifier
    {}
| typedef_name_spec
    {}

struct_or_union_specifier:
  struct_or_union option_general_identifier_ LBRACE struct_declaration_list RBRACE
    {}
| struct_or_union general_identifier
    {}

struct_or_union:
  STRUCT
    {}
| UNION
    {}

struct_declaration_list:
  struct_declaration
    {}
| struct_declaration_list struct_declaration
    {}

struct_declaration:
  specifier_qualifier_list option_struct_declarator_list_ SEMICOLON
    {}
| static_assert_declaration
    {}

specifier_qualifier_list:
  list_eq1_type_specifier_unique_type_qualifier_
    {}
| list_ge1_type_specifier_nonunique_type_qualifier_
    {}

struct_declarator_list:
  struct_declarator
    {}
| struct_declarator_list COMMA struct_declarator
    {}

struct_declarator:
  declarator
    {}
| option_declarator_ COLON constant_expression
    {}

enum_specifier:
  ENUM option_general_identifier_ LBRACE enumerator_list option_COMMA_ RBRACE
    {}
| ENUM general_identifier
    {}

enumerator_list:
  enumerator
    {}
| enumerator_list COMMA enumerator
    {}

enumerator:
  enumeration_constant
    {}
| enumeration_constant EQ constant_expression
    {}

enumeration_constant:
  general_identifier
    {}

atomic_type_specifier:
  ATOMIC LPAREN type_name RPAREN
    {}
| ATOMIC ATOMIC_LPAREN type_name RPAREN
    {}

type_qualifier:
  CONST
    {}
| RESTRICT
    {}
| VOLATILE
    {}
| ATOMIC
    {}

function_specifier:
  INLINE
    {}
| NORETURN
    {}

alignment_specifier:
  ALIGNAS LPAREN type_name RPAREN
    {}
| ALIGNAS LPAREN constant_expression RPAREN
    {}

declarator:
  direct_declarator
    {}
| pointer direct_declarator
    {}

direct_declarator:
  general_identifier
    {}
| LPAREN save_context declarator RPAREN
    {}
| direct_declarator LBRACK option_type_qualifier_list_ option_assignment_expression_ RBRACK
    {}
| direct_declarator LBRACK STATIC option_type_qualifier_list_ assignment_expression RBRACK
    {}
| direct_declarator LBRACK type_qualifier_list STATIC assignment_expression RBRACK
    {}
| direct_declarator LBRACK option_type_qualifier_list_ STAR RBRACK
    {}
| direct_declarator LPAREN scoped_parameter_type_list_ RPAREN
    {}
| direct_declarator LPAREN save_context option_identifier_list_ RPAREN
    {}

pointer:
  STAR option_type_qualifier_list_ option_pointer_
    {}

type_qualifier_list:
  option_type_qualifier_list_ type_qualifier
    {}

parameter_type_list:
  parameter_list option___anonymous_0_ save_context
    {}

parameter_list:
  parameter_declaration
    {}
| parameter_list COMMA parameter_declaration
    {}

parameter_declaration:
  declaration_specifiers declarator_varname
    {}
| declaration_specifiers option_abstract_declarator_
    {}

identifier_list:
  var_name
    {}
| identifier_list COMMA var_name
    {}

type_name:
  specifier_qualifier_list option_abstract_declarator_
    {}

abstract_declarator:
  pointer
    {}
| direct_abstract_declarator
    {}
| pointer direct_abstract_declarator
    {}

direct_abstract_declarator:
  LPAREN save_context abstract_declarator RPAREN
    {}
| option_direct_abstract_declarator_ LBRACK option_assignment_expression_ RBRACK
    {}
| option_direct_abstract_declarator_ LBRACK type_qualifier_list option_assignment_expression_ RBRACK
    {}
| option_direct_abstract_declarator_ LBRACK STATIC option_type_qualifier_list_ assignment_expression RBRACK
    {}
| option_direct_abstract_declarator_ LBRACK type_qualifier_list STATIC assignment_expression RBRACK
    {}
| option_direct_abstract_declarator_ LBRACK STAR RBRACK
    {}
| LPAREN option_scoped_parameter_type_list__ RPAREN
    {}
| direct_abstract_declarator LPAREN option_scoped_parameter_type_list__ RPAREN
    {}

c_initializer:
  assignment_expression
    {}
| LBRACE initializer_list option_COMMA_ RBRACE
    {}

initializer_list:
  option_designation_ c_initializer
    {}
| initializer_list COMMA option_designation_ c_initializer
    {}

designation:
  designator_list EQ
    {}

designator_list:
  option_designator_list_ designator
    {}

designator:
  LBRACK constant_expression RBRACK
    {}
| DOT general_identifier
    {}

static_assert_declaration:
  STATIC_ASSERT LPAREN constant_expression COMMA STRING_LITERAL RPAREN SEMICOLON
    {}

statement:
  labeled_statement
    {}
| scoped_compound_statement_
    {}
| expression_statement
    {}
| c99_scoped_selection_statement_
    {}
| c99_scoped_iteration_statement_
    {}
| jump_statement
    {}

labeled_statement:
  general_identifier COLON statement
    {}
| CASE constant_expression COLON statement
    {}
| DEFAULT COLON statement
    {}

compound_statement:
  LBRACE option_block_item_list_ RBRACE
    {}

block_item_list:
  option_block_item_list_ block_item
    {}

block_item:
  declaration
    {}
| statement
    {}

expression_statement:
  option_expression_ SEMICOLON
    {}

selection_statement:
  IF LPAREN expression RPAREN c99_scoped_statement_ ELSE c99_scoped_statement_
    {}
| IF LPAREN expression RPAREN c99_scoped_statement_ %prec below_ELSE
    {}
| SWITCH LPAREN expression RPAREN c99_scoped_statement_
    {}

iteration_statement:
  WHILE LPAREN expression RPAREN c99_scoped_statement_
    {}
| DO c99_scoped_statement_ WHILE LPAREN expression RPAREN SEMICOLON
    {}
| FOR LPAREN option_expression_ SEMICOLON option_expression_ SEMICOLON option_expression_ RPAREN c99_scoped_statement_
    {}
| FOR LPAREN declaration option_expression_ SEMICOLON option_expression_ RPAREN c99_scoped_statement_
    {}

jump_statement:
  GOTO general_identifier SEMICOLON
    {}
| CONTINUE SEMICOLON
    {}
| BREAK SEMICOLON
    {}
| RETURN option_expression_ SEMICOLON
    {}

translation_unit_file:
  external_declaration translation_unit_file
    {}
| external_declaration EOF
    {}

external_declaration:
  function_definition
    {}
| declaration
    {}

function_definition1:
  declaration_specifiers declarator_varname
    {}

function_definition:
  function_definition1 option_declaration_list_ compound_statement
    {}

declaration_list:
  declaration
    {}
| declaration_list declaration
    {}

%%
