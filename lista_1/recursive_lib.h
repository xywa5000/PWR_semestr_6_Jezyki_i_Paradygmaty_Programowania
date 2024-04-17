#ifndef RECURSIVE_LIB_H
#define RECURSIVE_LIB_H
#include <stdbool.h>
#include "struct_lib.h"

// Deklaracja funkcji rekurencyjnej obliczającej silnię
long long factorial_recursive(int n);

// Deklaracja funkcji rekurencyjnej obliczającej największy wspólny dzielnik (NWD)
unsigned int gcd_recursive(unsigned int a, unsigned int b);

// Deklaracja funkcji rekurencyjnej rozwiązującej liniowe równanie diofantyczne
Solution diophantine_recursive(int a, int b, int c);

// Deklaracja funkcji pomocniczej
Solution diophantine_helper_recursive(int a, int b, int c);

#endif /* RECURSIVE_LIB_H */
