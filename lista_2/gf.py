import math

ORDER = 1234577

# Sieve of Eratosthenes algorithm to find primes up to n
def sieve_of_eratosthenes(n):
    is_prime = [True] * (n + 1)
    is_prime[0] = False
    is_prime[1] = False
    p = 2
    while p * p <= n:
        if is_prime[p]:
            i = p * p
            while i <= n:
                is_prime[i] = False
                i += p
        p += 1
    
    primes = [i for i in range(n + 1) if is_prime[i]]
    return primes

# Function to find the prime factor and its exponent of a number
def power_of_prime(n):
    for i in sieve_of_eratosthenes(n):
        if n % i == 0:
            count = 0
            while n % i == 0:
                n //= i
                count += 1
            if n == 1:
                return i, count
            else:
                raise ValueError("Not a power of prime.")
    raise ValueError("Not a power of prime.")

# Class representing an element of a Galois field
class Gf:
    def __init__(self, value):
        self.value = value % ORDER

    # Getter for the value
    def getValue(self):
        return self.value

    # Compute the characteristic of the field
    def characteristic(self):
        prime, _ = power_of_prime(ORDER)
        return prime

    # Arithmetic operations
    def __add__(self, other):
        return Gf((self.value + other.value) % ORDER)

    def __iadd__(self, other):
        self.value = (self.value + other.value) % ORDER
        return self

    def __sub__(self, other):
        return Gf((self.value - other.value) % ORDER)

    def __isub__(self, other):
        self.value = (self.value - other.value) % ORDER
        return self

    def __mul__(self, other):
        return Gf((self.value * other.value) % ORDER)

    def __imul__(self, other):
        self.value = (self.value * other.value) % ORDER
        return self

    def inv(self):
        t, newt = 0, 1
        r, newr = ORDER, self.value
        while newr != 0:
            quotient = r // newr
            t, newt = newt, t - quotient * newt
            r, newr = newr, r - quotient * newr
        if r > 1:
            raise ValueError("Value is not invertible.")
        if t < 0:
            t += ORDER
        return Gf(t)

    def __truediv__(self, other):
        return self * other.inv()

    def __itruediv__(self, other):
        self *= other.inv()
        return self

    # Comparison operators
    def __eq__(self, other):
        return self.value == other.value

    def __ne__(self, other):
        return not (self == other)

    def __lt__(self, other):
        return self.value < other.value

    def __le__(self, other):
        return self.value <= other.value

    def __gt__(self, other):
        return self.value > other.value

    def __ge__(self, other):
        return self.value >= other.value

    # Output operator
    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return str(self)

if __name__ == "__main__":
    x, y = map(int, input("Enter two integers: ").split())
    print()

    a = Gf(x)
    b = Gf(y)

    print("a.characteristic():", a.characteristic())
    print("a.getValue():", a.getValue(), "\n")

    print("b.characteristic():", b.characteristic())
    print("b.getValue():", b.getValue(), "\n")

    print("a + b:", a + b)
    print("a - b:", a - b)
    print("a * b:", a * b)
    print("a / b:", a / b, "\n")

    print("a += b:", end=" ")
    a += b
    print(a)

    print("a -= b:", end=" ")
    a -= b
    print(a)

    print("a *= b:", end=" ")
    a *= b
    print(a)

    print("a /= b:", end=" ")
    a /= b
    print(a, "\n")

    print("a == b:", a == b)
    print("a != b:", a != b)
    print("a >= b:", a >= b)
    print("a <= b:", a <= b)
    print("a > b:", a > b)
    print("a < b:", a < b, "\n")

    print("a = b:", end=" ")
    a = b
    print(a)
