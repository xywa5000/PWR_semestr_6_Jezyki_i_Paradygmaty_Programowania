#include "ada_lib_wrapper.h"
#include <stdlib.h>

extern long long Factorial_Iterative_External_ada (int n);
extern long long Factorial_Recursive_External_ada (int n);

extern unsigned int GCD_Iterative_External_ada (unsigned int n, unsigned int m);
extern unsigned int GCD_Recursive_External_ada (unsigned int n, unsigned int m);

extern Solution Diophantine_Iterative_External_ada (int a, int b, int c);
extern Solution Diophantine_Recursive_External_ada (int a, int b, int c);

extern Solution Diophantine_Helper_Recursive_External_ada (int a, int b, int c);

long long factorial_iterative_ada (int n) {
    return Factorial_Iterative_External_ada(n);
}

unsigned int gcd_iterative_ada (unsigned int n, unsigned int m) {
    return GCD_Iterative_External_ada (n, m);
}

Solution diophantine_iterative_ada (int a, int b, int c) {
    return Diophantine_Iterative_External_ada (a, b, c);
}

long long factorial_recursive_ada (int n) {
    return Factorial_Recursive_External_ada(n);
}

unsigned int gcd_recursive_ada (unsigned int n, unsigned int m) {
    return GCD_Recursive_External_ada (n, m);
}

Solution diophantine_recursive_ada (int a, int b, int c) {
    return Diophantine_Recursive_External_ada (a, b, c);
}

Solution diophantine_helper_recursive_ada (int a, int b, int c) {
    return Diophantine_Helper_Recursive_External_ada (a, b, c);
}
