-- Funkcja primes zwracająca listę wszystkich liczb pierwszych między 2 a n
primes :: Integer -> [Integer]
primes n = sieve [2..n]
    where
        -- Funkcja pomocnicza sita Eratostenesa
        sieve [] = []
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main = do
    print(primes 10)
    print(primes 60)
    print(primes 100)
