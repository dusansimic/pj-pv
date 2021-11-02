
-- 1. Napisati funkciju "ispeglaj" koja prima listu ciji su elementi
--    liste [[Int]], a vraca listu brojeva [Int] tako sto sve elemente
--    podlisti spoji u jednu listu.
--
--    [[1, 2, 3], [4, 5], [6], [7, 8], [], [9]] -> [1, 2, 3, 4, 5, 6, 7, 8, 9]

import Outputable (whenPprDebug)
ispeglaj :: [[Int]] -> [Int]
ispeglaj [] = []
ispeglaj (l:ls) = l ++ ispeglaj ls

ispeglaj' :: [[Int]] -> [Int]
ispeglaj' = foldr (++) []

-- 2. Napisati funkciju koja prima listu listi brojeva [[Int]], i vraca
--    listu suma elemenata listi koje ona sadrzi. Koristiti funkciju fold.
--
--    [[1, 2, 3], [2, 3], [1, 2, 4], [5]] -> [6, 5, 7, 5]


sumirajListe :: [[Int]] -> [Int]
sumirajListe [] = []
sumirajListe (h:t) = foldr (+) 0 h : sumirajListe t

sumirajListe' :: [[Int]] -> [Int]
sumirajListe' l = [ sum ls | ls <- l ]

sumirajListe'' :: [[Int]] -> [Int]
sumirajListe'' = map sum

sumirajListe''' :: [[Int]] -> [Int]
sumirajListe''' [] = []
sumirajListe''' (h:t) = suma : sumirajListe t
  where
    suma = foldr (+) 0 h

-- 3. Napisati funkciju koja prima listu ciji su elementi liste brojeva,
--    te iz svake podliste izbacuje parne brojeve. Ako slucajno ostane
--    neka prazna lista, nju je potrebno izbaciti. Koristiti funkciju
--    filter.
--
--    [[1, 2, 3], [2, 4], [3, 4, 5], [7]] -> [[1, 3], [3, 5], [7]]

izbaciParne :: [[Int]] -> [[Int]]
izbaciParne [] = []
izbaciParne (h:t)
  | neparni == [] = izbaciParne t
  | otherwise = neparni : izbaciParne t
  where
    neparni = filter odd h

izbaciParne' :: [[Int]] -> [[Int]]
izbaciParne' l = map (filter odd) (filter (any odd) l)

-- 4. Napisati funkciju "okreni" koja prolazi kroz listu stringova, 
--    te pomocu funkcija map i reverse "okrene" sve stringove.

--    "neki string" -> "gnirts iken"

okreni :: [[Char]] -> [[Char]]
okreni = map reverse

-- 5. Napisati funkciju koja prihvata listu listi brojeva [[Int]] i 
--    iz svake podliste izbacuje elemente deljive sa 3.

izbaciDeljiveSa3 :: [[Int]] -> [[Int]]
izbaciDeljiveSa3 = map (filter (\a -> mod a 3 /= 0))

-- 6. Napisati funkciju koja prihvata listu listi brojeva, primenjuje na 
--    nju prethodnu funkciju i onda izbaci sve podliste koje imaju manje od 5 elemenata.

izbaciManjeOd5 :: [[Int]] -> [[Int]]
izbaciManjeOd5 l = filter (\a -> length a >= 5) (izbaciDeljiveSa3 l)
