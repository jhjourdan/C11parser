// declaration_ambiguity.c
typedef int T;
void f (void) {
  signed int;   // declares zero variable of type "signed int"
  signed T;     // declares a variable "T" of type "signed"
}
