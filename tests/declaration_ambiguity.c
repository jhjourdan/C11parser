// declaration_ambiguity.c
typedef int T;
void f (void) {
  unsigned int;   // declares zero variable of type "unsigned int"
  unsigned T;     // declares a variable "T" of type "unsigned"
}
