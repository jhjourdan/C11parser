  $ $TESTDIR/../parse < $TESTDIR/argument_scope.c
  $ $TESTDIR/../parse < $TESTDIR/atomic.c
  $ $TESTDIR/../parse < $TESTDIR/c11-noreturn.c
  $ $TESTDIR/../parse < $TESTDIR/c1x-alignas.c
  $ $TESTDIR/../parse < $TESTDIR/char-literal-printing.c
  $ $TESTDIR/../parse < $TESTDIR/c-namespace.c
  $ $TESTDIR/../parse < $TESTDIR/control-scope.c
  $ $TESTDIR/../parse < $TESTDIR/declarators.c
  $ $TESTDIR/../parse < $TESTDIR/designator.c
  $ $TESTDIR/../parse < $TESTDIR/expressions.c
  $ $TESTDIR/../parse < $TESTDIR/long-long-struct.c
  $ $TESTDIR/../parse < $TESTDIR/function-decls.c
  $ $TESTDIR/../parse < $TESTDIR/statements.c
  $ $TESTDIR/../parse < $TESTDIR/struct-recursion.c
  $ $TESTDIR/../parse < $TESTDIR/types.c
  $ $TESTDIR/../parse < $TESTDIR/local_typedef.c
  $ $TESTDIR/../parse < $TESTDIR/declaration_ambiguity.c
  $ $TESTDIR/../parse < $TESTDIR/declarator_visibility.c
  $ $TESTDIR/../parse < $TESTDIR/enum_shadows_typedef.c
  $ $TESTDIR/../parse < $TESTDIR/enum_constant_visibility.c
  $ $TESTDIR/../parse < $TESTDIR/namespaces.c
  $ $TESTDIR/../parse < $TESTDIR/local_scope.c
  $ $TESTDIR/../parse < $TESTDIR/block_scope.c
  $ $TESTDIR/../parse < $TESTDIR/if_scopes.c
  $ $TESTDIR/../parse < $TESTDIR/loop_scopes.c
  $ $TESTDIR/../parse < $TESTDIR/no_local_scope.c
  $ $TESTDIR/../parse < $TESTDIR/function_parameter_scope.c
  $ $TESTDIR/../parse < $TESTDIR/function_parameter_scope_extends.c
  $ $TESTDIR/../parse < $TESTDIR/dangling_else.c
SHOULD FAIL:
  $ $TESTDIR/../parse < $TESTDIR/dangling_else_misleading.fail.c
  Fatal error: exception Parser.Error
  [2]
  $ $TESTDIR/../parse < $TESTDIR/dangling_else_lookahead.c
  $ $TESTDIR/../parse < $TESTDIR/dangling_else_lookahead.if.c
  $ $TESTDIR/../parse < $TESTDIR/parameter_declaration_ambiguity.c
  $ $TESTDIR/../parse < $TESTDIR/parameter_declaration_ambiguity.test.c
  $ $TESTDIR/../parse < $TESTDIR/bitfield_declaration_ambiguity.c
SHOULD FAIL (does not because we do not have a semantic analysis):
  $ $TESTDIR/../parse < $TESTDIR/bitfield_declaration_ambiguity.fail.c
  $ $TESTDIR/../parse < $TESTDIR/bitfield_declaration_ambiguity.ok.c
  $ $TESTDIR/../parse < $TESTDIR/atomic_parenthesis.c
  $ ls $TESTDIR/*.c
  */tests/argument_scope.c (glob)
  */tests/atomic.c (glob)
  */tests/atomic_parenthesis.c (glob)
  */tests/bitfield_declaration_ambiguity.c (glob)
  */tests/bitfield_declaration_ambiguity.fail.c (glob)
  */tests/bitfield_declaration_ambiguity.ok.c (glob)
  */tests/block_scope.c (glob)
  */tests/c-namespace.c (glob)
  */tests/c11-noreturn.c (glob)
  */tests/c1x-alignas.c (glob)
  */tests/char-literal-printing.c (glob)
  */tests/control-scope.c (glob)
  */tests/dangling_else.c (glob)
  */tests/dangling_else_lookahead.c (glob)
  */tests/dangling_else_lookahead.if.c (glob)
  */tests/dangling_else_misleading.fail.c (glob)
  */tests/declaration_ambiguity.c (glob)
  */tests/declarator_visibility.c (glob)
  */tests/declarators.c (glob)
  */tests/designator.c (glob)
  */tests/enum-trick.c (glob)
  */tests/enum.c (glob)
  */tests/enum_constant_visibility.c (glob)
  */tests/enum_shadows_typedef.c (glob)
  */tests/expressions.c (glob)
  */tests/function-decls.c (glob)
  */tests/function_parameter_scope.c (glob)
  */tests/function_parameter_scope_extends.c (glob)
  */tests/if_scopes.c (glob)
  */tests/local_scope.c (glob)
  */tests/local_typedef.c (glob)
  */tests/long-long-struct.c (glob)
  */tests/loop_scopes.c (glob)
  */tests/namespaces.c (glob)
  */tests/no_local_scope.c (glob)
  */tests/parameter_declaration_ambiguity.c (glob)
  */tests/parameter_declaration_ambiguity.test.c (glob)
  */tests/statements.c (glob)
  */tests/struct-recursion.c (glob)
  */tests/typedef_star.c (glob)
  */tests/types.c (glob)
  */tests/variable_star.c (glob)
