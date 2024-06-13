-- Funkcja obliczająca największy wspólny dzielnik
gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- Funkcja totient obliczająca wartość funkcji Eulera dla n
totient :: Integer -> Integer
totient n = fromIntegral $ length [x | x <- [1..n-1], gcd' x n == 1]

main = do
    print(totient 10)
    print(totient 60)
    print(totient 100)
