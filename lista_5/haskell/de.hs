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

main = do
    print(de 14 30)
    print(de 21 35)
    print(de 35 19)
