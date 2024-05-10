# test_iterative_lib.py
from iterative_lib import factorial_iterative, gcd_iterative, diophantine_iterative

print("Biblioteka iteracyjna\n")

n = int(input("Podaj liczbę naturalną do obliczenia silni: "))
print(f"{n}! = {factorial_iterative(n)}")

a, b = map(int, input("\nPodaj dwie liczby naturalne do obliczenia NWD: ").split())
print(f"NWD({a}, {b}) = {gcd_iterative(a, b)}")

a, b, c = map(int, input("\nPodaj współczynniki a, b i c dla równania diofantycznego ax + by = c: ").split())
sol = diophantine_iterative(a, b, c)
print(f"Error: {sol.err}")
print(f"Rozwiązanie: x = {sol.x}, y = {sol.y}")
