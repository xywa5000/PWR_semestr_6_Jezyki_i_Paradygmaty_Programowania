#ifndef ADA_LIB_WRAPPER_H
#define ADA_LIB_WRAPPER_H
#include <stdbool.h>

typedef struct {
    int x;
    int y;
    bool err;
} Solution;

// Deklaracja funkcji iteracyjnej obliczającej silnię
long long factorial_iterative_ada(int n);

// Deklaracja funkcji iteracyjnej obliczającej największy wspólny dzielnik (NWD)
unsigned int gcd_iterative_ada(unsigned int a, unsigned int b);

// Deklaracja funkcji iteracyjnej rozwiązującej liniowe równanie diofantyczne
Solution diophantine_iterative_ada(int a, int b, int c);

// Deklaracja funkcji rekurencyjnej obliczającej silnię
long long factorial_recursive_ada(int n);

// Deklaracja funkcji rekurencyjnej obliczającej największy wspólny dzielnik (NWD)
unsigned int gcd_recursive_ada(unsigned int a, unsigned int b);

// Deklaracja funkcji rekurencyjnej rozwiązującej liniowe równanie diofantyczne
Solution diophantine_recursive_ada(int a, int b, int c);

// Deklaracja funkcji rekurencyjnej rozwiązującej liniowe równanie diofantyczne
Solution diophantine_helper_recursive_ada(int a, int b, int c);

#endif /* ADA_LIB_WRAPPER_H */