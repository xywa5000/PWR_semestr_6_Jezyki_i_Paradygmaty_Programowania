# c_library_wrapper.py
import ctypes
from ctypes import Structure, c_int, c_bool, c_longlong


# Struktura do przechowywania rozwiązania równania diofantycznego
class Solution(Structure):
    _fields_ = [("x", c_int),
                ("y", c_int),
                ("err", c_bool)]


# Ładowanie biblioteki C
c_iterative_lib = ctypes.CDLL('./iterative_lib.so')
c_recursive_lib = ctypes.CDLL('./recursive_lib.so')

# Deklaracja typów argumentów i zwracanych wartości dla funkcji biblioteki C
c_iterative_lib.factorial_iterative.argtypes = [c_int]
c_iterative_lib.factorial_iterative.restype = c_longlong

c_recursive_lib.factorial_recursive.argtypes = [c_int]
c_recursive_lib.factorial_recursive.restype = c_longlong

c_iterative_lib.gcd_iterative.argtypes = [c_int, c_int]
c_iterative_lib.gcd_iterative.restype = c_int

c_recursive_lib.gcd_recursive.argtypes = [c_int, c_int]
c_recursive_lib.gcd_recursive.restype = c_int

c_iterative_lib.diophantine_iterative.argtypes = [c_int, c_int, c_int]
c_iterative_lib.diophantine_iterative.restype = Solution

c_recursive_lib.diophantine_recursive.argtypes = [c_int, c_int, c_int]
c_recursive_lib.diophantine_recursive.restype = Solution


# Funkcja wrapper dla funkcji iteracyjnej obliczającej silnię
def factorial_iterative_c(n):
    return c_iterative_lib.factorial_iterative(n)


# Funkcja wrapper dla funkcji iteracyjnej obliczającej NWD
def gcd_iterative_c(a, b):
    return c_iterative_lib.gcd_iterative(a, b)


# Funkcja wrapper dla funkcji iteracyjnej rozwiązującej równanie diofantyczne
def diophantine_iterative_c(a, b, c):
    return c_iterative_lib.diophantine_iterative(a, b, c)


# Funkcja wrapper dla funkcji rekurencyjnej obliczającej silnię
def factorial_recursive_c(n):
    return c_recursive_lib.factorial_recursive(n)


# Funkcja wrapper dla funkcji rekurencyjnej obliczającej NWD
def gcd_recursive_c(a, b):
    return c_recursive_lib.gcd_recursive(a, b)


# Funkcja wrapper dla funkcji rekurencyjnej rozwiązującej równanie diofantyczne
def diophantine_recursive_c(a, b, c):
    return c_recursive_lib.diophantine_recursive(a, b, c)
