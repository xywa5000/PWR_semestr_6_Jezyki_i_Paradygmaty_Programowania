# recursive_lib.py
from struct_lib import Solution


# Funkcja rekurencyjna obliczająca silnię
def factorial_recursive(n):
    if n < 0:
        return -1
    elif n == 0 or n == 1:
        return 1
    else:
        return n * factorial_recursive(n - 1)


# Funkcja rekurencyjna obliczająca NWD
def gcd_recursive(a, b):
    if b == 0:
        return a
    else:
        return gcd_recursive(b, a % b)


# Funkcja rekurencyjna rozwiązująca równanie diofantyczne
def diophantine_recursive(a, b, c):
    sol = Solution(0, 0, False)
    
    # Obliczenie największego wspólnego dzielnika
    d = gcd_recursive(a, b)
    
    # Sprawdzenie, czy c jest podzielne przez d
    if c % d != 0:
        # Brak rozwiązania
        sol.err = True
        return sol
    
    # Rekurencyjne wywołanie funkcji pomocniczej do obliczenia współczynników x i y
    return diophantine_helper_recursive(a, b, c)


# Funkcja pomocnicza do obliczenia współczynników x i y przy pomocy rekurencyjnego algorytmu Euklidesa
def diophantine_helper_recursive(a, b, c):
    sol = Solution(0, 0, False)
    
    # Warunek kończący rekurencję
    if b == 0:
        sol.x = c // a
        sol.y = 0
        return sol
    
    # Rekurencyjne wywołanie funkcji dla reszty dzielenia a przez b
    sol = diophantine_helper_recursive(b, a % b, c)
    
    # Aktualizacja współczynników x i y
    temp = sol.x
    sol.x = sol.y
    sol.y = temp - (a // b) * sol.y
    
    return sol
