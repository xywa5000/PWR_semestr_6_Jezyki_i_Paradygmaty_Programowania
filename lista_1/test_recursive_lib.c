#include <stdio.h>
#include "recursive_lib.h"
#include "struct_lib.h"

int main() {
    printf("Biblioteka rekurencyjna \n\n");
    unsigned int n;
    printf("Podaj liczbe naturalna do obliczenia silni: ");
    scanf("%d", &n);
    printf("%d! = %lld\n", n, factorial_recursive(n));

    unsigned int a, b;
    printf("\nPodaj dwie liczby naturalne do obliczenia NWD: ");
    scanf("%u %u", &a, &b);
    printf("NWD(%u, %u) = %u\n", a, b, gcd_recursive(a, b));

    int x, y, c;
    printf("\nPodaj wspolczynniki a, b i c dla rownania diofantycznego ax + by = c: ");
    scanf("%d %d %d", &a, &b, &c);
    Solution sol = diophantine_recursive(a, b, c);
    printf("Error: %s\n", sol.err ? "true" : "false");
    printf("Rozwiazanie: x = %d, y = %d\n", sol.x, sol.y);

    return 0;
}
