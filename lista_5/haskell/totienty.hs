import Data.List (nub)


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
    let examples = [10, 12345, 9999999]
    putStrLn "Przykłady użycia funkcji totient:"
    mapM_ (\n -> putStrLn $ "totient " ++ show n ++ " => " ++ show (totient n)) examples

    putStrLn "\nPrzykłady użycia funkcji totient2:"
    mapM_ (\n -> putStrLn $ "totient2 " ++ show n ++ " => " ++ show (totient2 n)) examples
