#ifndef PYTHON_LIB_WRAPPER_H
#define PYTHON_LIB_WRAPPER_H
#include <stdbool.h>
#include <Python.h>

typedef struct {
    int x;
    int y;
    bool err;
} Solution;

// Deklaracja funkcji iteracyjnej obliczającej silnię
long long factorial_iterative_python(int n);

// Deklaracja funkcji iteracyjnej obliczającej największy wspólny dzielnik (NWD)
unsigned int gcd_iterative_python(unsigned int a, unsigned int b);

// Deklaracja funkcji iteracyjnej rozwiązującej liniowe równanie diofantyczne
Solution diophantine_iterative_python(int a, int b, int c);

// Deklaracja funkcji rekurencyjnej obliczającej silnię
long long factorial_recursive_python(int n);

// Deklaracja funkcji rekurencyjnej obliczającej największy wspólny dzielnik (NWD)
unsigned int gcd_recursive_python(unsigned int a, unsigned int b);

// Deklaracja funkcji rekurencyjnej rozwiązującej liniowe równanie diofantyczne
Solution diophantine_recursive_python(int a, int b, int c);

#endif /* PYTHON_LIB_WRAPPER_H */