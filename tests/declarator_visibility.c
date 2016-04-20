// declarator_visibility.c
typedef int T, T1(T);   // T is visible when declaring T1.
void f(void) {
  int (*T)(T x) = 0;
    // During the declaration of the parameter x, T is still a
    // typedef name, as the declarator of T has not yet ended.
  int T1 = sizeof((int)T1);
    // In the initializer, the declarator of T1 has ended, so
    // T1 denotes a variable.
}
