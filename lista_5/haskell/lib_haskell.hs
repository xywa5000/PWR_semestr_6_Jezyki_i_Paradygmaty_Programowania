import Data.Array
import Data.List (nub)

-- (1) binomial

binomial :: Integer -> Integer -> Integer
binomial n k
    | k < 0 || k > n = 0
    | k == 0 || k == n = 1
    | otherwise = dp ! (n, k)
    where
        dp :: Array (Integer, Integer) Integer
        dp = array ((0, 0), (n, n)) [((i, j), binomial' i j) | i <- [0..n], j <- [0..n]]
        binomial' :: Integer -> Integer -> Integer
        binomial' i 0 = 1
        binomial' i j
            | j == i = 1
            | otherwise = dp ! (i-1, j) + dp ! (i-1, j-1)

-- (2) binomial2

binomial2 :: Integer -> Integer -> Integer
binomial2 n k
    | k < 0 || k > n = 0
    | otherwise = pascal !! (fromIntegral n) !! (fromIntegral k)
    where
        pascal = iterate nextRow [1]
        nextRow :: [Integer] -> [Integer]
        nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])

-- (3) mergesort

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs =
    let (left, right) = splitAt (length xs `div` 2) xs
    in merge (mergesort left) (mergesort right)

-- (4) de

extendedEuclid :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclid a 0 = (a, 1, 0)
extendedEuclid a b =
    let (gcd, x1, y1) = extendedEuclid b (a `mod` b)
    in (gcd, y1, x1 - (a `div` b) * y1)


de :: Integer -> Integer -> (Integer, Integer, Integer)
de a b =
    let (gcdAB, x, y) = extendedEuclid a b
    in (x * (z `div` gcdAB), y * (z `div` gcdAB), z)
    where z = gcd a b

-- (5) prime_factors

prime_factors :: Integer -> [Integer]
prime_factors n = prime_factorsHelper n 2


prime_factorsHelper :: Integer -> Integer -> [Integer]
prime_factorsHelper 1 _ = []
prime_factorsHelper n factor
    | n `mod` factor == 0 = factor : prime_factorsHelper (n `div` factor) factor
    | otherwise = prime_factorsHelper n (factor + 1)

-- (6) totient

gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)


totient :: Integer -> Integer
totient n = fromIntegral $ length [x | x <- [1..n-1], gcd' x n == 1]

-- (7) totient2

totient2 :: Integer -> Integer
totient2 n =
    let factors = prime_factors n
        uniqueFactors = nub factors
        phiFactors = product [ p - 1 | p <- uniqueFactors ]
        phiN = n * phiFactors `div` product uniqueFactors
    in phiN

-- (8) primes

primes :: Integer -> [Integer]
primes n = sieve [2..n]
    where
        sieve [] = []
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main :: IO ()
main = do
    let result = (binomial 10 3)
    putStrLn $ "(binomial 10 3) = " ++ show result
    let result = (binomial 11 5)
    putStrLn $ "(binomial 11 5) = " ++ show result
    let result = (binomial 195 5)
    putStrLn $ "(binomial 195 5) = " ++ show result

    let result = (binomial2 10 3)
    putStrLn $ "(binomial2 10 3) = " ++ show result
    let result = (binomial2 11 5)
    putStrLn $ "(binomial2 11 5) = " ++ show result
    let result = (binomial2 195 5)
    putStrLn $ "(binomial2 195 5) = " ++ show result

    let result = (mergesort [4, 1, 3, 2])
    putStrLn $ "(mergesort [4, 1, 3, 2]) = " ++ show result
    let result = (mergesort [8, 4, 2, 7, 1, 6, 3, 5])
    putStrLn $ "(mergesort [8, 4, 2, 7, 1, 6, 3, 5]) = " ++ show result
    let result = (mergesort [2, 4, 1, 3, 3, 4, 2, 1])
    putStrLn $ "(mergesort [2, 4, 1, 3, 3, 4, 2, 1]) = " ++ show result
    
    let result = (de 35 15)
    putStrLn $ "(de 35 15) = " ++ show result
    let result = (de 123 72)
    putStrLn $ "(de 123 72) = " ++ show result
    let result = (de 195 555)
    putStrLn $ "(de 195 555) = " ++ show result

    let result = (prime_factors 100)
    putStrLn $ "(prime_factors 100) = " ++ show result
    let result = (prime_factors 1234)
    putStrLn $ "(prime_factors 1234) = " ++ show result
    let result = (prime_factors 185130)
    putStrLn $ "(prime_factors 185130) = " ++ show result

    let result = (totient 100)
    putStrLn $ "(totient 100) = " ++ show result
    let result = (totient 1234)
    putStrLn $ "(totient 1234) = " ++ show result
    let result = (totient 185130)
    putStrLn $ "(totient 185130) = " ++ show result

    let result = (totient2 100)
    putStrLn $ "(totient2 100) = " ++ show result
    let result = (totient2 1234)
    putStrLn $ "(totient2 1234) = " ++ show result
    let result = (totient2 185130)
    putStrLn $ "(totient2 185130) = " ++ show result

    let result = (primes 10)
    putStrLn $ "(primes 10) = " ++ show result
    let result = (primes 50)
    putStrLn $ "(primes 50) = " ++ show result
    let result = (primes 150)
    putStrLn $ "(primes 150) = " ++ show result
