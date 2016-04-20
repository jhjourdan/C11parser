// local_scope.c
typedef int T;
void f(void) {
  if(1) {
    int T;
    T = 1; // T is a variable
  }
  T x; // T is a type, again
  x = 1;
}
