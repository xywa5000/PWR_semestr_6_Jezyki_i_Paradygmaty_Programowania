#include <stdio.h>
#include "recursive_lib.h"

int main() {
    unsigned int n;
    printf("Biblioteka rekurencyjna \n\n");
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
    printf("Rozwiazanie: x = %d, y = %d\n", sol.x, sol.y);

    printf("========================================\n");

    sol = diophantine(3, 7, 12);
    printf("Rozwiazanie: x = %d, y = %d\n", sol.x, sol.y);

    sol = diophantine(13, 6, 11);
    printf("Rozwiazanie: x = %d, y = %d\n", sol.x, sol.y);

    sol = diophantine(9, 2, 1);
    printf("Rozwiazanie: x = %d, y = %d\n", sol.x, sol.y);

    sol = diophantine(7, 17, 9);
    printf("Rozwiazanie: x = %d, y = %d\n", sol.x, sol.y);

    sol = diophantine(12, 15, 17);
    printf("Rozwiazanie: x = %d, y = %d\n", sol.x, sol.y);

    return 0;
}
