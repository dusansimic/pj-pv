jeKvadratna :: [[a]] -> Bool
jeKvadratna m = x*x == y && y'
  where
    x = length m
    y = length (concat m)
    y' = allEq [ length r | r <- m ]

allEq :: Eq a => [a] -> Bool
allEq [] = False
allEq (h:t) = allEq' h t
  where
    allEq' :: Eq a => a -> [a] -> Bool
    allEq' _ [] = True
    allEq' a (h:t) = a == h && allEq' h t

minor :: Int -> [[a]] -> [[a]]
minor i m = [ remove (i - 1) r | r <- m ]

remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 (x:xs) = xs
remove n (x:xs) = x : remove (n-1) xs

racun' :: Num a => [[a]] -> a
racun' m
  | length m > 2 = racun3' m
  | otherwise = racun2' m

racun2' :: Num a => [[a]] -> a
racun2' [[a, b], [c, d]] = a*d - b*c

racun3' :: Num a => [[a]] -> a
racun3' m = sum [ ((-1)^(i + 1)) * (fr !! (i - 1)) * racun' (minor i rs) | i <- [1..n] ]
  where
    fr = head m
    rs = tail m
    n = length m

-- Ovo je ulazna funkcija u program
-- Prosledjuje se kvadratna matrica i vraca se njena determinanta
-- Ukoliko matrica nije kvadratna, vraca se 666
det :: Num a => [[a]] -> a
det m
  | jeKvadratna m = racun' m
  | otherwise = 666
