#include "recursive_lib.h"
#include <stdlib.h>
#include <stdio.h>

// Implementacja funkcji rekurencyjnej obliczającej silnię
long long factorial_recursive(int n) {
    if (n < 0)
        return -1;
    else if (n == 0 || n == 1)
        return 1;
    else
        return n * factorial_recursive(n - 1);
}

// Implementacja funkcji rekurencyjnej obliczającej NWD
unsigned int gcd_recursive(unsigned int a, unsigned int b) {
    if (b == 0)
        return a;
    else
        return gcd_recursive(b, a % b);
}

// Implementacja funkcji rekurencyjnej rozwiązującej równanie diofantyczne
Solution diophantine_recursive(int a, int b, int c) {
    Solution sol = {0, 0, false};
    
    // Obliczenie największego wspólnego dzielnika
    int d = gcd_recursive(a, b);
    
    // Sprawdzenie, czy c jest podzielne przez d
    if (c % d != 0) {
        // Brak rozwiązania
        sol.err = true;
        return sol;
    }
    
    // Rekurencyjne wywołanie funkcji pomocniczej do obliczenia współczynników x i y
    return diophantine_helper_recursive(a, b, c);
}

// Funkcja pomocnicza do obliczenia współczynników x i y przy pomocy rekurencyjnego algorytmu Euklidesa
Solution diophantine_helper_recursive(int a, int b, int c) {
    Solution sol = {0, 0, false};
    
    // Warunek kończący rekurencję
    if (b == 0) {
        sol.x = c / a;
        sol.y = 0;
        return sol;
    }
    
    // Rekurencyjne wywołanie funkcji dla reszty dzielenia a przez b
    sol = diophantine_helper_recursive(b, a % b, c);
    
    // Aktualizacja współczynników x i y
    int temp = sol.x;
    sol.x = sol.y;
    sol.y = temp - (a / b) * sol.y;
    
    return sol;
}
