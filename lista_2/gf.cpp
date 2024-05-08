#include <iostream>
#include <stdexcept>
#include <vector>
#include <cmath>

constexpr unsigned long long ORDER = 1234577;

// Sieve of Eratosthenes algorithm to find primes up to n
std::vector<int> sieve_of_eratosthenes(int n) {
    std::vector<bool> is_prime(n + 1, true);
    is_prime[0] = false;
    is_prime[1] = false;
    int p = 2;
    while (p * p <= n) {
        if (is_prime[p]) {
            int i = p * p;
            while (i <= n) {
                is_prime[i] = false;
                i += p;
            }
        }
        p += 1;
    }

    std::vector<int> primes;
    for (int i = 0; i <= n; ++i) {
        if (is_prime[i]) {
            primes.push_back(i);
        }
    }
    return primes;
}

// Function to find the prime factor and its exponent of a number
std::pair<int, int> power_of_prime(int n) {
    for (int i : sieve_of_eratosthenes(n)) {
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

// Class representing an element of a Galois field
class Gf {
private:
    unsigned long long value;

    // Calculate the modular inverse of the element
    Gf inv() const {
        long long t = 0;
        long long newt = 1;
        long long r = ORDER;
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
            t += ORDER;
        }
        return Gf(t);
    }

public:
    // Public constructor
    Gf(unsigned long long value) : value(value % ORDER) {}

    // Getter for the value
    unsigned long long getValue() const {
        return value;
    }

    // Compute the characteristic of the field
    unsigned long long characteristic() const {
        auto [prime, _] = power_of_prime(static_cast<int>(ORDER));
        return prime;
    }
    
    // Comparison operators
    friend bool operator==(const Gf& lhs, const Gf& rhs) {
        return lhs.value == rhs.value;
    }

    friend bool operator!=(const Gf& lhs, const Gf& rhs) {
        return !(lhs == rhs);
    }

    friend bool operator<(const Gf& lhs, const Gf& rhs) {
        return lhs.value < rhs.value;
    }

    friend bool operator<=(const Gf& lhs, const Gf& rhs) {
        return lhs.value <= rhs.value;
    }

    friend bool operator>(const Gf& lhs, const Gf& rhs) {
        return lhs.value > rhs.value;
    }

    friend bool operator>=(const Gf& lhs, const Gf& rhs) {
        return lhs.value >= rhs.value;
    }

    // Output operator
    friend std::ostream& operator<<(std::ostream& os, const Gf& gf) {
        return os << gf.value;
    }

    // Arithmetic operations
    Gf operator+(const Gf& other) const {
        return Gf((value + other.value) % ORDER);
    }

    Gf& operator+=(const Gf& other) {
        value = (value + other.value) % ORDER;
        return *this;
    }

    Gf operator-(const Gf& other) const {
        return Gf((value + ORDER - other.value) % ORDER);
    }

    Gf& operator-=(const Gf& other) {
        value = (value + ORDER - other.value) % ORDER;
        return *this;
    }

    Gf operator*(const Gf& other) const {
        return Gf((value * other.value) % ORDER);
    }

    Gf& operator*=(const Gf& other) {
        value = (value * other.value) % ORDER;
        return *this;
    }

    Gf operator/(const Gf& other) const {
        return *this * other.inv();
    }

    Gf& operator/=(const Gf& other) {
        *this *= other.inv();
        return *this;
    }

    // Assignment operator
    Gf& operator=(const Gf& other) {
        value = other.value % ORDER;
        return *this;
    }
};

int main() {

    int x, y;

    std::cout << "Enter two integers: ";
    std::cin >> x >> y;
    std::cout << "\n";

    Gf a(x);
    Gf b(y);
    
    std::cout << "a.characteristic(): " << a.characteristic() << "\n";
    std::cout << "a.getValue(): " << a.getValue() << "\n\n";

    std::cout << "b.characteristic(): " << b.characteristic() << "\n";
    std::cout << "b.getValue(): " << b.getValue() << "\n\n";
    
    std::cout << "a + b: " << a + b << "\n";
    std::cout << "a - b: " << a - b << "\n";
    std::cout << "a * b: " << a * b << "\n";
    std::cout << "a / b: " << a / b << "\n\n";

    std::cout << "a += b: " << (a += b) << "\n";
    std::cout << "a -= b: " << (a -= b) << "\n";
    std::cout << "a *= b: " << (a *= b) << "\n";
    std::cout << "a /= b: " << (a /= b) << "\n\n";

    std::cout << "a == b: " << ((a == b) ? "true" : "false") << "\n";
    std::cout << "a != b: " << ((a != b) ? "true" : "false") << "\n";
    std::cout << "a >= b: " << ((a >= b) ? "true" : "false") << "\n";
    std::cout << "a <= b: " << ((a <= b) ? "true" : "false") << "\n";
    std::cout << "a > b: " << ((a > b) ? "true" : "false") << "\n";
    std::cout << "a < b: " << ((a < b) ? "true" : "false") << "\n\n";

    std::cout << "a = b: " << (a = b) << "\n";
    
    return 0;
}
