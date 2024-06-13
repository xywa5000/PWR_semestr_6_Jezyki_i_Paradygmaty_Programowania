import Data.Array
import Data.List (nub)

-- Funkcja obliczająca dwumian Newtona (n, k)
binomial :: Integer -> Integer -> Integer
binomial n k
    | k < 0 || k > n = 0  -- warunek brzegowy: jeśli k jest spoza zakresu [0, n], wynik jest 0
    | k == 0 || k == n = 1  -- warunek brzegowy: (n, 0) = (n, n) = 1
    | otherwise = dp ! (n, k)
    where
        -- Używamy tablicy do przechowywania wyników dla zapamiętywania obliczeń (memoizacja)
        dp :: Array (Integer, Integer) Integer
        dp = array ((0, 0), (n, n)) [((i, j), binomial' i j) | i <- [0..n], j <- [0..n]]
        
        -- Funkcja pomocnicza do obliczania dwumianu Newtona za pomocą rekurencji
        binomial' :: Integer -> Integer -> Integer
        binomial' i 0 = 1
        binomial' i j
            | j == i = 1
            | otherwise = dp ! (i-1, j) + dp ! (i-1, j-1)

-- Funkcja obliczająca dwumian Newtona (n, k) wykorzystująca trójkąt Pascala
binomial2 :: Integer -> Integer -> Integer
binomial2 n k
    | k < 0 || k > n = 0  -- warunek brzegowy: jeśli k jest spoza zakresu [0, n], wynik jest 0
    | otherwise = pascal !! (fromIntegral n) !! (fromIntegral k)
    where
        -- Tworzymy trójkąt Pascala jako lista list
        pascal = iterate nextRow [1]
        
        -- Funkcja generująca kolejny wiersz trójkąta Pascala na podstawie poprzedniego
        nextRow :: [Integer] -> [Integer]
        nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])

-- Funkcja pomocnicza do scalania dwóch posortowanych list
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- Funkcja mergesort sortująca listę przez scalanie
mergesort :: Ord a => [a] -> [a]
mergesort [] = []   -- pusta lista jest już posortowana
mergesort [x] = [x] -- lista jednoelementowa jest już posortowana
mergesort xs =
    let (left, right) = splitAt (length xs `div` 2) xs
    in merge (mergesort left) (mergesort right)

-- Funkcja pomocnicza do rozszerzonego algorytmu Euklidesa
extendedEuclid :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclid a 0 = (a, 1, 0)
extendedEuclid a b =
    let (gcd, x1, y1) = extendedEuclid b (a `mod` b)
    in (gcd, y1, x1 - (a `div` b) * y1)

-- Funkcja de rozwiązująca równanie de ax + by = z
de :: Integer -> Integer -> (Integer, Integer, Integer)
de a b =
    let (gcdAB, x, y) = extendedEuclid a b
    in (x * (z `div` gcdAB), y * (z `div` gcdAB), z)
    where z = gcd a b

prime_factors :: Integer -> [Integer]
prime_factors n = prime_factorsHelper n 2

-- pomocnicza funkcja rekurencyjna, która używa dzielenia i sprawdza kolejne liczby jako potencjalne czynniki pierwsze
prime_factorsHelper :: Integer -> Integer -> [Integer]
prime_factorsHelper 1 _ = []  -- gdy liczba zostanie rozłożona na czynniki pierwsze, kończymy
prime_factorsHelper n factor
    | n `mod` factor == 0 = factor : prime_factorsHelper (n `div` factor) factor
    | otherwise = prime_factorsHelper n (factor + 1)

-- Funkcja obliczająca największy wspólny dzielnik
gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- Funkcja totient obliczająca wartość funkcji Eulera dla n
totient :: Integer -> Integer
totient n = fromIntegral $ length [x | x <- [1..n-1], gcd' x n == 1]

-- Funkcja obliczająca wartość funkcji Eulera na podstawie czynników pierwszych liczby n
totient2 :: Integer -> Integer
totient2 n =
    let factors = prime_factors n
        uniqueFactors = nub factors  -- usuwamy powtórzenia
        phiFactors = product [ p - 1 | p <- uniqueFactors ]  -- obliczamy część mnożenia
        phiN = n * phiFactors `div` product uniqueFactors  -- obliczamy wynik
    in phiN

-- Funkcja primes zwracająca listę wszystkich liczb pierwszych między 2 a n
primes :: Integer -> [Integer]
primes n = sieve [2..n]
    where
        -- Funkcja pomocnicza sita Eratostenesa
        sieve [] = []
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main = do
    print(binomial 195 5)
    print(binomial2 195 5)
    print(mergesort [8, 4, 2, 7, 1, 6, 3, 5])
    print(de 35 15)
    print(prime_factors 100)
    print(totient 100)
    print(totient2 100)
    print(primes 100)
