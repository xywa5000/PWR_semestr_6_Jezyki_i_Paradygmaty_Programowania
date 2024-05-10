# iterative_lib.py
from struct_lib import Solution


# Funkcja obliczająca silnię iteracyjnie
def factorial_iterative(n):
    if n < 0:
        return -1
    result = 1
    for i in range(2, n + 1):
        result *= i
    return result


# Funkcja obliczająca NWD iteracyjnie
def gcd_iterative(a, b):
    while b != 0:
        a, b = b, a % b
    return a


# Funkcja rozwiązująca równanie diofantyczne iteracyjnie
def diophantine_iterative(a, b, c):
    sol = Solution(0, 0, False)
    
    gcd_ab = gcd_iterative(a, b)
    if c % gcd_ab != 0:
        # Brak rozwiązania, gdy c nie jest podzielne przez NWD(a, b)
        sol.err = True
        return sol
    
    # Obliczenie współczynników x i y przy pomocy rozszerzonego algorytmu Euklidesa
    x, y = 1, 0
    x1, y1 = 0, 1
    
    while b != 0:
        q = a // b
        r = a % b
        
        x, x1 = x1, x - q * x1
        y, y1 = y1, y - q * y1
        
        a, b = b, r
    
    # Obliczenie rozwiązania dla równania diofantycznego
    k = c // gcd_iterative(a, b)
    sol.x = x * k
    sol.y = y * k
    
    return sol
