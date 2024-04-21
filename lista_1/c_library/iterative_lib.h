#ifndef ITERATIVE_LIB_H
#define ITERATIVE_LIB_H
#include <stdbool.h>
#include "struct_lib.h"

// Deklaracja funkcji rekurencyjnej obliczającej silnię
long long factorial_iterative(int n);

// Deklaracja funkcji rekurencyjnej obliczającej największy wspólny dzielnik (NWD)
unsigned int gcd_iterative(unsigned int a, unsigned int b);

// Deklaracja funkcji rekurencyjnej rozwiązującej liniowe równanie diofantyczne
Solution diophantine_iterative(int a, int b, int c);

#endif /* ITERATIVEE_LIB_H */
