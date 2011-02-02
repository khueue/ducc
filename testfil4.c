// Only declaration.
int foo0(int a, int b);

// Only definition.
int foo1(int a, int b) {}

// Match.
int foo2(int a, int b);
int foo2(int a, int b) {}

// Match.
int foo3(int a,  int b);
int foo3(int aa, int bb) {}

// ERROR: Conflicting types.
int foo4(int a, int b);
//int foo4(int a, char b) {}

// ERROR: Conflicting types.
int foo5(int a, int b);
//int foo5(int a, int b[]) {}

// ERROR: Different arity. 
int foo6(int a, int b);
//int foo6(int a, int b, int c) {}

// ERROR: Different return type. (XXX Currently unhandled.)
int  foo7(int a, int b);
char foo7(int a, int b) {}

// Match. (XXX magically works in analyze_fundec, not properly handled)
int foo8(int a) {}
int foo8(int a);

// ERROR: Different arity. (XXX unhandled in analyze_fundec)
int foo9(int a, int b) {}
int foo9(int b);