#include "iterative_lib.h"
#include <stdlib.h>

// Implementacja funkcji iteracyjnej obliczającej silnię
long long factorial_iterative(int n) {
    if (n < 0)
        return -1;
    long long result = 1;
    for (int i = 2; i <= n; ++i) {
        result *= i;
    }
    return result;
}

// Implementacja funkcji iteracyjnej obliczającej NWD
unsigned int gcd_iterative(unsigned int a, unsigned int b) {
    while (b != 0) {
        unsigned int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

// Implementacja funkcji iteracyjnej rozwiązującej równanie diofantyczne
Solution diophantine_iterative(int a, int b, int c) {
    Solution sol = {0, 0, false};
    
    int gcd_ab = gcd_iterative(a, b);
    if (c % gcd_ab != 0) {
        // Brak rozwiązania, gdy c nie jest podzielne przez NWD(a, b)
        sol.err = true;
        return sol;
    }
    
    // Obliczenie współczynników x i y przy pomocy rozszerzonego algorytmu Euklidesa
    int x = 1, y = 0;
    int x1 = 0, y1 = 1;
    int temp;
    
    while (b != 0) {
        int q = a / b;
        int r = a % b;
        
        temp = x1;
        x1 = x - q * x1;
        x = temp;
        
        temp = y1;
        y1 = y - q * y1;
        y = temp;
        
        a = b;
        b = r;
    }
    
    // Obliczenie rozwiązania dla równania diofantycznego
    int k = c / gcd_iterative(a, b);
    sol.x = x * k;
    sol.y = y * k;
    
    return sol;
}
