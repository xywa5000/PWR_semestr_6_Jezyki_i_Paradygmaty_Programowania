import Data.Array

-- Funkcja obliczająca dwumian Newtona (n, k)
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

main = do
    print(binomial 5 2)
    print(binomial 10 3)
    print(binomial 6 4)
