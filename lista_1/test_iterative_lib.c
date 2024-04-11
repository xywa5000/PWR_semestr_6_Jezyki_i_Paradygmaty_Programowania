#include <stdio.h>
#include "iterative_lib.h"

int main() {
    unsigned int n;
    printf("Biblioteka iteracyjna \n\n");
    printf("Podaj liczbe naturalna do obliczenia silni: ");
    scanf("%u", &n);
    printf("%u! = %llu\n", n, factorial(n));

    unsigned int a, b;
    printf("\nPodaj dwie liczby naturalne do obliczenia NWD: ");
    scanf("%u %u", &a, &b);
    printf("NWD(%u, %u) = %u\n", a, b, gcd(a, b));

    int x, y, c;
    printf("\nPodaj wspolczynniki a, b i c dla rownania diofantycznego ax + by = c: ");
    scanf("%d %d %d", &a, &b, &c);
    Solution sol = diophantine(a, b, c);
    printf("Error: %s\n", sol.err ? "true" : "false");
    printf("Rozwiazanie: x = %d, y = %d\n", sol.x, sol.y);

    return 0;
}
