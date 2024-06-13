prime_factors :: Integer -> [Integer]
prime_factors n = prime_factorsHelper n 2

-- pomocnicza funkcja rekurencyjna, która używa dzielenia i sprawdza kolejne liczby jako potencjalne czynniki pierwsze
prime_factorsHelper :: Integer -> Integer -> [Integer]
prime_factorsHelper 1 _ = []  -- gdy liczba zostanie rozłożona na czynniki pierwsze, kończymy
prime_factorsHelper n factor
    | n `mod` factor == 0 = factor : prime_factorsHelper (n `div` factor) factor
    | otherwise = prime_factorsHelper n (factor + 1)

main = do
    print(prime_factors 60)
    print(prime_factors 123456)
    print(prime_factors 17)
