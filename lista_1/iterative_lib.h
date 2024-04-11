#ifndef ITERATIVE_LIB_H
#define ITERATIVE_LIB_H
#include <stdbool.h>

// Struktura do przechowywania rozwiązania równania diofantycznego
typedef struct {
    int x;
    int y;
    bool err;
} Solution;

// Deklaracja funkcji rekurencyjnej obliczającej silnię
unsigned long long factorial(unsigned int n);

// Deklaracja funkcji rekurencyjnej obliczającej największy wspólny dzielnik (NWD)
unsigned int gcd(unsigned int a, unsigned int b);

// Deklaracja funkcji rekurencyjnej rozwiązującej liniowe równanie diofantyczne
Solution diophantine(int a, int b, int c);

#endif /* ITERATIVEE_LIB_H */
