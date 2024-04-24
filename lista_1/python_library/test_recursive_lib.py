# test_recursive_lib.py
from recursive_lib import factorial_recursive, gcd_recursive, diophantine_recursive

print("Biblioteka rekurencyjna\n")

n = int(input("Podaj liczbę naturalną do obliczenia silni: "))
print(f"{n}! = {factorial_recursive(n)}")

a, b = map(int, input("\nPodaj dwie liczby naturalne do obliczenia NWD: ").split())
print(f"NWD({a}, {b}) = {gcd_recursive(a, b)}")

a, b, c = map(int, input("\nPodaj współczynniki a, b i c dla równania diofantycznego ax + by = c: ").split())
sol = diophantine_recursive(a, b, c)
print(f"Error: {sol.err}")
print(f"Rozwiązanie: x = {sol.x}, y = {sol.y}")
