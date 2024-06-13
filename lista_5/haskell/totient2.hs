import Data.List (nub)

-- Funkcja prime_factors z prime_factors.hs
prime_factors :: Integer -> [Integer]
prime_factors n = prime_factorsHelper n 2

-- pomocnicza funkcja rekurencyjna, która używa dzielenia i sprawdza kolejne liczby jako potencjalne czynniki pierwsze
prime_factorsHelper :: Integer -> Integer -> [Integer]
prime_factorsHelper 1 _ = []  -- gdy liczba zostanie rozłożona na czynniki pierwsze, kończymy
prime_factorsHelper n factor
    | n `mod` factor == 0 = factor : prime_factorsHelper (n `div` factor) factor
    | otherwise = prime_factorsHelper n (factor + 1)

-- Funkcja obliczająca wartość funkcji Eulera na podstawie czynników pierwszych liczby n
totient2 :: Integer -> Integer
totient2 n =
    let factors = prime_factors n
        uniqueFactors = nub factors  -- usuwamy powtórzenia
        phiFactors = product [ p - 1 | p <- uniqueFactors ]  -- obliczamy część mnożenia
        phiN = n * phiFactors `div` product uniqueFactors  -- obliczamy wynik
    in phiN

main = do
    print(totient2 10)
    print(totient2 60)
    print(totient2 100)
