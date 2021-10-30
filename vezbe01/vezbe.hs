-- 1. Napisati funkciju koja uklanja poslednji element liste.

ukloniPoslednjiElement :: [Int] -> [Int]
ukloniPoslednjiElement [] = []
ukloniPoslednjiElement [a] = []
ukloniPoslednjiElement (a:as) = a : ukloniPoslednjiElement as

-- 2. Napisati funkciju koja uklanja pretposlednji element liste.

ukloniPretposlednji :: [Int] -> [Int]
ukloniPretposlednji [] = []
ukloniPretposlednji [a, b] = [b]
ukloniPretposlednji (a:as) = a : ukloniPretposlednji as

-- 3. Izracunati n!
--      a) rekurzivno
--      b) repno rekurzivno

faktorijel :: Int -> Int
faktorijel 0 = 1
faktorijel n = n * faktorijel (n - 1)

faktorijelRep :: Int -> Int
faktorijelRep n = faktorijelRep' n 1

faktorijelRep' :: Int -> Int -> Int
faktorijelRep' 0 accum = accum
faktorijelRep' n accum = faktorijelRep' (n - 1) (accum * n)

-- 4. Napisati funkciju "imaVelikaSlova" koja proverava da li neki string ima velika slova

imaVelikaSlova :: [Char] -> Bool
imaVelikaSlova [] = False
imaVelikaSlova (s:sx)
  | s >= 'A' && s <= 'Z' = True
  -- | isAsciiUpper s = True
  | otherwise  = imaVelikaSlova sx


spljosti :: [Int] -> [Int]
spljosti [] = []
spljosti [a] = [a]
spljosti (a:b:x)
  | a == b = spljosti(a:x)
  | otherwise = a : spljosti(b:x)

-- 6. Napisati funkciju koja iz liste stringova izbacuje one koji imaju sva mala slova

izbaciSveMale :: [[Char]] -> [[Char]]
izbaciSveMale [] = []
izbaciSveMale (s:sx)
  | imaVelikaSlova s = s : izbaciSveMale sx
  | otherwise = izbaciSveMale sx

-- 7. Napisati funkciju koja kvadrira sve elemente liste (preko ZF izraza).

kvadriraj :: [Int] -> [Int]
kvadriraj a = [ b * b | b <- a ]

-- 8. Napisati funkciju jeDeljiv, koji prima 2 broja i vraca true ako prvi broj moze da deli
-- drugi.

jeDeljiv :: Int -> Int -> Bool
jeDeljiv a b = mod b a == 0

-- 9. Napisati funkciju jeDeljivSa3 koristeci prethodno definisanu funkciju

jeDeljivSa3 :: Int -> Bool
jeDeljivSa3 = jeDeljiv 3


-- 10. Napisati funkciju filter' f l, koja filtrira elemente liste l pomocu funkcije f.

filter' :: (Int -> Bool) -> [Int] -> [Int]
filter' _ [] = []
filter' f (e:ex)
  | f e = e : filter' f ex
  | otherwise = filter' f ex

-- filter' (\x -> mod x 2 == 0) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

-- 11. Definisati funkciju deljiviSa3 koristeci prethodne 2 funkcije.

deljiviSa3 :: [Int] -> [Int]
deljiviSa3 = filter' jeDeljivSa3
