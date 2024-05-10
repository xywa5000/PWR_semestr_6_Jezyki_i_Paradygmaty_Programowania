import java.math.BigInteger;
import java.util.Random;
import java.util.function.Function;
import java.util.ArrayList;
import java.util.List;

public class dh {

    public static void main(String[] args) {
        FiniteFieldElement a = new FiniteFieldElement(19);
        FiniteFieldElement m = new FiniteFieldElement(2115);

        DiffieHellmanSetup<FiniteFieldElement> dh = new DiffieHellmanSetup<>(FiniteFieldElement::new);
        System.out.println("Generator: " + dh.getGenerator());

        // Utwórz dwóch użytkowników - Alice i Bob
        User<FiniteFieldElement> alice = new User<>(dh);
        User<FiniteFieldElement> bob = new User<>(dh);

        // Uzyskaj klucze publiczne dla Alice i Boba
        FiniteFieldElement alicePubKey = alice.getPublicKey();
        FiniteFieldElement bobPubKey = bob.getPublicKey();

        System.out.println("Alice Public Key: " + alicePubKey);
        System.out.println("Bob Public Key: " + bobPubKey);

        // Wymień klucze publiczne między Alice i Bobem
        alice.setReceiverPublicKey(bobPubKey);
        bob.setReceiverPublicKey(alicePubKey);

        // Zaszyfruj i odszyfruj wiadomość przez Alice i Boba
        System.out.println("Message: " + m);
        FiniteFieldElement c = alice.encrypt(m);
        System.out.println("Encrypted by Alice: " + c);
        FiniteFieldElement mDecrypted = bob.decrypt(c);
        System.out.println("Decrypted by Bob: " + mDecrypted);
    }

    private static final long CHARACTERISTIC = 1234577;

    public interface AlgebraicBody<T extends AlgebraicBody<T>> {
        T multiplyBy(T other);
        T divideBy(T other);
        T fromLong(long value);
    }

    public static boolean check(long suspect) {
        long i = 2;
        long tmp = suspect;

        while (i * i <= tmp) {
            if (tmp % i == 0) {
                if (BigInteger.valueOf(suspect).pow((int)((CHARACTERISTIC - 1) / i)).mod(BigInteger.valueOf(CHARACTERISTIC)).equals(BigInteger.ONE)) {
                    return false;
                }
                tmp /= i;
            } else {
                i += 1;
            }
        }
        if (tmp > 1 && BigInteger.valueOf(suspect).pow((int)((CHARACTERISTIC - 1) / tmp)).mod(BigInteger.valueOf(CHARACTERISTIC)).equals(BigInteger.ONE)) {
            return false;
        }
        return true;
    }

    public static class DiffieHellmanSetup<T extends AlgebraicBody<T>> {
        private T generator;

        public DiffieHellmanSetup(Function<Long, T> constructor) {
            Random rng = new Random();
            long generatorValue = rng.nextInt((int) CHARACTERISTIC - 1) + 1;
            while (!check(generatorValue)) {
                generatorValue = rng.nextInt((int) CHARACTERISTIC - 1) + 1;
            }
            this.generator = constructor.apply(generatorValue);
        }

        public T getGenerator() {
            return generator;
        }

        public T power(T a, long b) {
            T res = a;
            while (b > 0) {
                if (b % 2 == 1) {
                    res = res.multiplyBy(a);
                }
                a = a.multiplyBy(a);
                b /= 2;
            }
            return res;
        }
    }

    public static class User<T extends AlgebraicBody<T>> {
        private long secret;
        private DiffieHellmanSetup<T> dhsetup;
        private T key;
        private T receiverPublicKey;

        public User(DiffieHellmanSetup<T> dhsetup) {
            this.dhsetup = dhsetup;
            this.secret = new Random().nextLong();
            System.out.println("Secret: " + secret);
        }

        public T getPublicKey() {
            return dhsetup.power(dhsetup.getGenerator(), secret);
        }

        public void setReceiverPublicKey(T publicKey) {
            this.key = dhsetup.power(publicKey, secret);
            System.out.println("Key: " + key);
        }

        public T encrypt(T m) {
            return m.multiplyBy(key);
        }

        public T decrypt(T c) {
            return c.divideBy(key);
        }
    }

    public static class FiniteFieldElement implements AlgebraicBody<FiniteFieldElement> {
        private static final long ORDER = 1234577;
        private long value;

        public FiniteFieldElement(long value) {
            this.value = value % ORDER;
        }

        public long getValue() {
            return value;
        }

        public long characteristic() {
            long[] primePower = powerOfPrime(ORDER);
            return primePower[0];
        }

        private FiniteFieldElement inv() {
            long t = 0;
            long newt = 1;
            long r = ORDER;
            long newr = value;
            while (newr != 0) {
                long quotient = r / newr;
                long tmp = newt;
                newt = t - quotient * newt;
                t = tmp;
                tmp = newr;
                newr = r - quotient * newr;
                r = tmp;
            }
            if (r > 1) {
                throw new ArithmeticException(value + " is not invertible");
            }
            if (t < 0) {
                t += ORDER;
            }
            return new FiniteFieldElement(t);
        }

        public FiniteFieldElement add(FiniteFieldElement other) {
            return new FiniteFieldElement((value + other.value) % ORDER);
        }

        public FiniteFieldElement subtract(FiniteFieldElement other) {
            return new FiniteFieldElement((value + ORDER - other.value) % ORDER);
        }

        public FiniteFieldElement multiply(FiniteFieldElement other) {
            return new FiniteFieldElement((value * other.value) % ORDER);
        }

        public FiniteFieldElement divide(FiniteFieldElement other) {
            return multiply(other.inv());
        }

        private static long[] powerOfPrime(long n) {
            for (long i : sieveOfEratosthenes(n)) {
                if (n % i == 0) {
                    long count = 0;
                    while (n % i == 0) {
                        n /= i;
                        count++;
                    }
                    if (n == 1) {
                        return new long[]{i, count};
                    } else {
                        throw new ArithmeticException("Not a power of prime");
                    }
                }
            }
            throw new ArithmeticException("Not a power of prime");
        }

        private static List<Long> sieveOfEratosthenes(long n) {
            List<Boolean> isPrime = new ArrayList<>();
            for (int i = 0; i <= n; i++) {
                isPrime.add(true);
            }
            isPrime.set(0, false);
            isPrime.set(1, false);
            int p = 2;
            while (p * p <= n) {
                if (isPrime.get(p)) {
                    int i = p * p;
                    while (i <= n) {
                        isPrime.set(i, false);
                        i += p;
                    }
                }
                p++;
            }
            List<Long> primes = new ArrayList<>();
            for (int i = 0; i <= n; i++) {
                if (isPrime.get(i)) {
                    primes.add((long) i);
                }
            }
            return primes;
        }

        @Override
        public String toString() {
            return String.valueOf(value);
        }

        @Override
        public FiniteFieldElement multiplyBy(FiniteFieldElement other) {
            return multiply(other);
        }

        @Override
        public FiniteFieldElement divideBy(FiniteFieldElement other) {
            return divide(other);
        }

        @Override
        public FiniteFieldElement fromLong(long value) {
            return new FiniteFieldElement(value);
        }
    }
}
