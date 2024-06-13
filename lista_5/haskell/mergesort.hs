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

main = do
    print(mergesort [5,2,7,1,9,3])
