#include <iostream>
#include <random>
#include <vector>
#include <cmath>
#include <stdexcept>

// Constants defining the prime field
const long PRIME_CHARACTERISTIC = 1234577;
const long PRIME_ORDER = 1234577;

// Function to check if a number is a probable prime
bool isProbablePrime(unsigned long suspect) {
    unsigned long i = 2;
    unsigned long tmp = suspect;

    while (i * i <= tmp) {
        if (tmp % i == 0) {
            if (std::pow(suspect, (PRIME_CHARACTERISTIC - 1) / i) == 1) {
                return false;
            }
            tmp /= i;
        } else {
            i += 1;
        }
    }
    if (tmp > 1 && std::pow(suspect, (PRIME_CHARACTERISTIC - 1) / tmp) == 1) {
        return false;
    }
    return true;
}

// Sieve of Eratosthenes algorithm to generate prime numbers
std::vector<int> sieveOfEratosthenes(int n) {
    std::vector<bool> isPrime(n + 1, true);
    isPrime[0] = false;
    isPrime[1] = false;
    int p = 2;
    while (p * p <= n) {
        if (isPrime[p]) {
            int i = p * p;
            while (i <= n) {
                isPrime[i] = false;
                i += p;
            }
        }
        p += 1;
    }

    std::vector<int> primes;
    for (int i = 0; i <= n; ++i) {
        if (isPrime[i]) {
            primes.push_back(i);
        }
    }
    return primes;
}

// Function to factorize a number into its prime factors and their powers
std::pair<int, int> factorizeIntoPrimePowers(int n) {
    for (int i : sieveOfEratosthenes(n)) {
        if (n % i == 0) {
            int count = 0;
            while (n % i == 0) {
                n /= i;
                count += 1;
            }
            if (n == 1) {
                return std::make_pair(i, count);
            } else {
                throw std::runtime_error("Not a power of prime.");
            }
        }
    }
    throw std::runtime_error("Not a power of prime.");
}

// Class representing elements of a finite field
class FiniteFieldElement {
private:
    unsigned long long value;

    // Method returning the inverse of an element in the field
    FiniteFieldElement inverse() const {
        long long t = 0;
        long long newt = 1;
        long long r = PRIME_ORDER;
        long long newr = static_cast<long long>(value);
        while (newr != 0) {
            long long quotient = r / newr;
            long long tmp = newt;
            newt = t - quotient * newt;
            t = tmp;
            tmp = newr;
            newr = r - quotient * newr;
            r = tmp;
        }
        if (r > 1) {
            throw std::runtime_error("Value is not invertible.");
        }
        if (t < 0) {
            t += PRIME_ORDER;
        }
        return FiniteFieldElement(t);
    }

public:
    // Constructors
    FiniteFieldElement() : value(0) {}
    FiniteFieldElement(unsigned long long value) : value(value % PRIME_ORDER) {}

    // Method returning the value of the element
    unsigned long long getValue() const {
        return value;
    }

    // Method returning the characteristic of the field
    unsigned long long characteristic() const {
        auto [prime, _] = factorizeIntoPrimePowers(static_cast<int>(PRIME_ORDER));
        return prime;
    }
    
    // Overloaded arithmetic operators and comparisons
    // Addition
    FiniteFieldElement operator+(const FiniteFieldElement& other) const {
        return FiniteFieldElement((value + other.value) % PRIME_ORDER);
    }

    FiniteFieldElement& operator+=(const FiniteFieldElement& other) {
        value = (value + other.value) % PRIME_ORDER;
        return *this;
    }

    // Subtraction
    FiniteFieldElement operator-(const FiniteFieldElement& other) const {
        return FiniteFieldElement((value + PRIME_ORDER - other.value) % PRIME_ORDER);
    }

    FiniteFieldElement& operator-=(const FiniteFieldElement& other) {
        value = (value + PRIME_ORDER - other.value) % PRIME_ORDER;
        return *this;
    }

    // Multiplication
    FiniteFieldElement operator*(const FiniteFieldElement& other) const {
        return FiniteFieldElement((value * other.value) % PRIME_ORDER);
    }

    FiniteFieldElement& operator*=(const FiniteFieldElement& other) {
        value = (value * other.value) % PRIME_ORDER;
        return *this;
    }

    // Division
    FiniteFieldElement operator/(const FiniteFieldElement& other) const {
        return *this * other.inverse();
    }

    FiniteFieldElement& operator/=(const FiniteFieldElement& other) {
        *this *= other.inverse();
        return *this;
    }

    // Overloaded output operator for stream
    friend std::ostream& operator<<(std::ostream& os, const FiniteFieldElement& element) {
        return os << element.value;
    }
};

// Class representing the Diffie-Hellman key exchange setup
template <class T>
class DiffieHellmanSetup {
private:
    T generator;

public:
    DiffieHellmanSetup() {
        std::random_device rd;
        std::mt19937 rng(rd());
        std::uniform_int_distribution<unsigned long> dist(1, PRIME_CHARACTERISTIC - 1);

        unsigned long g = dist(rng);
        while (!isProbablePrime(g)) {
            g = dist(rng);
        }
        generator = T(g);
    }

    T getGenerator() {
        return generator;
    }

    T power(T a, unsigned long b) {
        T res = a;
        while (b > 0) {
            if (b % 2 == 1) {
                res = res * a;
            }
            a = a * a;
            b /= 2;
        }
        return res;
    }
};

// Class representing a user in the Diffie-Hellman key exchange
template <class T>
class DHUser {
private:
    long secret;
    DiffieHellmanSetup<T>* dhSetup;
    T key;

public:
    DHUser(DiffieHellmanSetup<T>* dhSetup) {
        std::random_device rd;
        std::mt19937 rng(rd());
        std::uniform_int_distribution<unsigned long> dist(1, PRIME_CHARACTERISTIC - 1);
        secret = dist(rng);
        this->dhSetup = dhSetup;
        std::cout << "Secret: " << secret << std::endl;
    }

    T getPublicKey() {
        return dhSetup->power(dhSetup->getGenerator(), secret);
    }

    void setKey(T a) {
        key = dhSetup->power(a, secret);
        std::cout << "Key: " << key.getValue() << std::endl;
    }

    T encrypt(T m) {
        return m * key;
    }

    T decrypt(T c) {
        return c / key;
    }
};

int main() {
    // Initialize the generator and message
    FiniteFieldElement generator(11);
    FiniteFieldElement message(2985);

    // Initialize the Diffie-Hellman configuration
    DiffieHellmanSetup<FiniteFieldElement> dhSetup;
    std::cout << "Generator: " << dhSetup.getGenerator().getValue() << std::endl;

    // Initialize the users
    DHUser<FiniteFieldElement> alice(&dhSetup);
    DHUser<FiniteFieldElement> bob(&dhSetup);

    // Alice's perspective
    std::cout << "Alice's Perspective:" << std::endl;
    FiniteFieldElement alicePublicKey = alice.getPublicKey();
    std::cout << "Alice's Public Key Sent: " << alicePublicKey.getValue() << std::endl;
    bob.setKey(alicePublicKey);

    // Bob's perspective
    std::cout << std::endl << "Bob's Perspective:" << std::endl;
    FiniteFieldElement bobPublicKey = bob.getPublicKey();
    std::cout << "Bob's Public Key Sent: " << bobPublicKey.getValue() << std::endl;
    alice.setKey(bobPublicKey);

    // Encrypt and decrypt messages
    std::cout << std::endl << "Alice's Perspective:" << std::endl;
    std::cout << "Message: " << message.getValue() << std::endl;
    FiniteFieldElement encryptedMessageAlice = alice.encrypt(message);
    std::cout << "Encrypted Message: " << encryptedMessageAlice.getValue() << std::endl;
    FiniteFieldElement decryptedMessageAlice = alice.decrypt(encryptedMessageAlice);
    std::cout << "Decrypted Message: " << decryptedMessageAlice.getValue() << std::endl;

    std::cout << std::endl << "Bob's Perspective:" << std::endl;
    std::cout << "Message: " << message.getValue() << std::endl;
    FiniteFieldElement encryptedMessageBob = bob.encrypt(message);
    std::cout << "Encrypted Message: " << encryptedMessageBob.getValue() << std::endl;
    FiniteFieldElement decryptedMessageBob = bob.decrypt(encryptedMessageBob);
    std::cout << "Decrypted Message: " << decryptedMessageBob.getValue() << std::endl;

    return 0;
}
