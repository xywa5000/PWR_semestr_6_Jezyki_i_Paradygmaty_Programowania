import Data.List (nub)
import Data.Array


binomial :: Integer -> Integer -> Integer
binomial n k
    | k < 0 || k > n = 0  -- warunek brzegowy: jeśli k jest spoza zakresu [0, n], wynik jest 0
    | k == 0 || k == n = 1  -- warunek brzegowy: (n, 0) = (n, n) = 1
    | otherwise = dp ! (n, k)
    where
        -- Używamy tablicy do przechowywania wyników dla zapamiętywania obliczeń
        dp :: Array (Integer, Integer) Integer
        dp = array ((0, 0), (n, n)) [((i, j), binomial' i j) | i <- [0..n], j <- [0..n]]
        
        -- Funkcja pomocnicza do obliczania dwumianu Newtona za pomocą rekurencji
        binomial' :: Integer -> Integer -> Integer
        binomial' i 0 = 1
        binomial' i j
            | j == i = 1
            | otherwise = dp ! (i-1, j) + dp ! (i-1, j-1)


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


prime_factors :: Integer -> [Integer]
prime_factors n = prime_factorsHelper n 2


prime_factorsHelper :: Integer -> Integer -> [Integer]
prime_factorsHelper 1 _ = []
prime_factorsHelper n factor
    | n `mod` factor == 0 = factor : prime_factorsHelper (n `div` factor) factor
    | otherwise = prime_factorsHelper n (factor + 1)


gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)


totient :: Integer -> Integer
totient n = fromIntegral $ length [x | x <- [1..n-1], gcd' x n == 1]


totient2 :: Integer -> Integer
totient2 n =
    let factors = prime_factors n
        uniqueFactors = nub factors
        phiFactors = product [ p - 1 | p <- uniqueFactors ]
        phiN = n * phiFactors `div` product uniqueFactors
    in phiN


main :: IO ()
main = do
    let binomial_examples = [(10, 3), (100, 10), (5000, 10)]
    
    putStrLn "Funkcja binomial:"
    mapM_ (\(a, b) -> putStrLn $ "binomial " ++ show a ++ " " ++ show b ++ " => " ++ show (binomial a b)) binomial_examples

    putStrLn "\nFunkcja binomial2:"
    mapM_ (\(a, b) -> putStrLn $ "binomial2 " ++ show a ++ " " ++ show b ++ " => " ++ show (binomial2 a b)) binomial_examples

    let totient_examples = [10, 12345, 9999999]
    putStrLn "\nFunkcja totient:"
    mapM_ (\n -> putStrLn $ "totient " ++ show n ++ " => " ++ show (totient n)) totient_examples

    putStrLn "\nFunkcji totient2:"
    mapM_ (\n -> putStrLn $ "totient2 " ++ show n ++ " => " ++ show (totient2 n)) totient_examples
