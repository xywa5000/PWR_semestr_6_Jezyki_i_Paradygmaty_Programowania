import random
from typing import Callable

class FiniteFieldElement:
    ORDER = 1234577

    def __init__(self, value):
        self.value = value % self.ORDER

    def characteristic(self):
        return self.power_of_prime(self.ORDER)[0]

    def inv(self):
        t, newt, r, newr = 0, 1, self.ORDER, self.value
        while newr != 0:
            quotient = r // newr
            t, newt = newt, t - quotient * newt
            newr, r = r - quotient * newr, newr
        if r > 1:
            raise ArithmeticError(f"{self.value} is not invertible")
        if t < 0:
            t += self.ORDER
        return FiniteFieldElement(t)

    def __add__(self, other):
        return FiniteFieldElement((self.value + other.value) % self.ORDER)

    def __sub__(self, other):
        return FiniteFieldElement((self.value - other.value) % self.ORDER)

    def __mul__(self, other):
        return FiniteFieldElement((self.value * other.value) % self.ORDER)

    def __truediv__(self, other):
        return self * other.inv()

    def __str__(self):
        return str(self.value)

    def power_of_prime(self, n):
        for i in self.sieve_of_eratosthenes(n):
            if n % i == 0:
                count = 0
                while n % i == 0:
                    n //= i
                    count += 1
                if n == 1:
                    return i, count
                else:
                    raise ArithmeticError("Not a power of prime")
        raise ArithmeticError("Not a power of prime")

    def sieve_of_eratosthenes(self, n):
        is_prime = [True] * (n + 1)
        is_prime[0] = is_prime[1] = False
        p = 2
        while p * p <= n:
            if is_prime[p]:
                i = p * p
                while i <= n:
                    is_prime[i] = False
                    i += p
            p += 1
        return [i for i in range(n + 1) if is_prime[i]]

class DiffieHellmanSetup:
    CHARACTERISTIC = 1234577

    def __init__(self, constructor: Callable[[int], FiniteFieldElement]):
        self.constructor = constructor
        self.generator = self.get_generator()

    def get_generator(self):
        generator_value = random.randint(1, self.CHARACTERISTIC - 1)
        while not self.check(generator_value):
            generator_value = random.randint(1, self.CHARACTERISTIC - 1)
        return self.constructor(generator_value)

    def power(self, a: FiniteFieldElement, b: int):
        res = a
        while b > 0:
            if b % 2 == 1:
                res *= a
            a *= a
            b //= 2
        return res

    def check(self, suspect):
        i, tmp = 2, suspect
        while i * i <= tmp:
            if tmp % i == 0:
                if pow(suspect, (self.CHARACTERISTIC - 1) // i, self.CHARACTERISTIC) == 1:
                    return False
                tmp //= i
            else:
                i += 1
        if tmp > 1 and pow(suspect, (self.CHARACTERISTIC - 1) // tmp, self.CHARACTERISTIC) == 1:
            return False
        return True

class User:
    def __init__(self, name: str, dh_setup: DiffieHellmanSetup):
        self.name = name
        self.dh_setup = dh_setup
        self.secret = random.randint(0, 2**32)

    def get_public_key(self):
        return self.dh_setup.power(self.dh_setup.generator, self.secret)

    def set_key(self, a: FiniteFieldElement):
        self.key = self.dh_setup.power(a, self.secret)

    def encrypt(self, m: FiniteFieldElement):
        return m * self.key

    def decrypt(self, c: FiniteFieldElement):
        return c / self.key

# Utwórz obiekt DiffieHellmanSetup
dh = DiffieHellmanSetup(FiniteFieldElement)

# Utwórz użytkowników Alice i Bob
alice = User("Alice", dh)
bob = User("Bob", dh)

# Klucz publiczny Alica
alice_pub_key = alice.get_public_key()

# Klucz publiczny Boba
bob_pub_key = bob.get_public_key()

# Ustawienie klucza dla Alice i Bob
alice.set_key(bob_pub_key)
bob.set_key(alice_pub_key)

# Wspólny klucz
common_key = alice.key

# Wiadomość
message = FiniteFieldElement(2115)

# Zaszyfrowana wiadomość dla Alice
encrypted_message_alice = alice.encrypt(message)

# Odszyfrowana wiadomość dla Alice
decrypted_message_alice = alice.decrypt(encrypted_message_alice)

# Zaszyfrowana wiadomość dla Boba
encrypted_message_bob = bob.encrypt(message)

# Odszyfrowana wiadomość dla Boba
decrypted_message_bob = bob.decrypt(encrypted_message_bob)

# Wyświetlanie wyników
print("Perspektywa Alice:")
print("Generator:", dh.generator)
print("Klucz publiczny wysyłany:", alice_pub_key)
print("Klucz publiczny odbierany:", bob_pub_key)
print("Wspólny klucz obliczony przez Alice i Boba:", common_key)
print("Wiadomość:", message)
print("Zaszyfrowana wiadomość:", encrypted_message_alice)
print("Odszyfrowana wiadomość:", decrypted_message_alice)
print()

print("Perspektywa Boba:")
print("Generator:", dh.generator)
print("Klucz publiczny wysyłany:", bob_pub_key)
print("Klucz publiczny odbierany:", alice_pub_key)
print("Wspólny klucz obliczony przez Alice i Boba:", common_key)
print("Wiadomość:", message)
print("Zaszyfrowana wiadomość:", encrypted_message_bob)
print("Odszyfrowana wiadomość:", decrypted_message_bob)
