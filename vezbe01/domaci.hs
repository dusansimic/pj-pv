{-# LANGUAGE ParallelListComp #-}
-- 1. Napisati funkciju koja uklanja svaki N-ti element liste.

ukloniNti :: Int -> [Int] -> [Int]
ukloniNti = ukloniNti' 1

ukloniNti' :: Int -> Int -> [Int] -> [Int]
ukloniNti' _ _ [] = []
ukloniNti' k n (l:lx)
  | k == n = ukloniNti' 1 n lx
  | otherwise = l : ukloniNti' (k + 1) n lx

-- 2. Napisati funkciju koja vraca listu delilaca nekog pozitivnog broja.

listaDelilaca :: Int -> [Int]
listaDelilaca n = n : listaDelilaca' n (div n 2)

listaDelilaca' :: Int -> Int -> [Int]
listaDelilaca' _ 1 = []
listaDelilaca' n k
  | mod n k == 0 = k : listaDelilaca' n (k - 1)
  | otherwise = listaDelilaca' n (k - 1)

-- 3. Napisati quicksort.

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (a:as) = quickSort [ e | e <- as, e < a ] ++ [a] ++ quickSort [ e | e <- as, e >= a ]

-- 4 Definisati filter'' f l preko ZF izraza

filter'' :: (Int -> Bool) -> [Int] -> [Int]
filter'' f l = [ e | e <- l, f e ]

-- 5. Napisati funkciju koja sumira sve elemente liste.

sumiraj :: [Int] -> Int
sumiraj [] = 0
sumiraj (a:as) = a + sumiraj as

sumiraj' :: [Int] -> Int
sumiraj' = foldr (+) 0

sumiraj'' :: [Int] -> Int
sumiraj'' = sum

-- 6. Napisati funkciju zip' koja prima 2 liste i sabira elemente na istim indeksima. 

zip' :: [Int] -> [Int] -> [Int]
zip' a b = [ ae + be | ae <- a | be <- b ]

zip'' :: [Int] -> [Int] -> [Int]
zip'' [] _ = []
zip'' _ [] = []
zip'' (a:as) (b:bs) = (a + b) : zip'' as bs

-- 7. Napisati funkciju koja sumira sve cifre prosledjenog broja:
--   a. rekurzivno
--   b. repno rekurzivno

sumirajCifre :: Int -> Int
sumirajCifre 0 = 0
sumirajCifre n = mod n 10 + sumirajCifre (div n 10)

sumirajCifreRep :: Int -> Int
sumirajCifreRep n = sumirajCifreRep' n 0

sumirajCifreRep' :: Int -> Int -> Int
sumirajCifreRep' 0 s = s
sumirajCifreRep' n s = sumirajCifreRep' (div n 10) (s + mod n 10)

-- 8. Sumirati parne cifre nekog broja

sumirajParneCifre :: Int -> Int
sumirajParneCifre 0 = 0
sumirajParneCifre n
  | even n = mod n 10 + sumirajParneCifre (div n 10)
  | otherwise = sumirajParneCifre (div n 10)
