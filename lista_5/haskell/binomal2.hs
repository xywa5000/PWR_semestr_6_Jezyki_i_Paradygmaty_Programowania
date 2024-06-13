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

main = do
    print(binomial2 5 2)
    print(binomial2 10 3)
    print(binomial2 6 4)
