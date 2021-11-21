{-
1. Napisati funkciju koja prihvata String i razdvoji ga
   po nekom karakteru. 

   rastaviString :: Char -> String -> [String]

   primer: "ana voli milovana" -> "ana", "voli", "milovana"
-}

rastaviString :: Char -> String -> [String]
rastaviString = split ""
  where
    split :: String -> Char -> String -> [String]
    split as _ [] = [as]
    split as delimiter (sh:st)
      | sh == delimiter = as : split "" delimiter st
      | otherwise = split (as ++ [sh]) delimiter st

{-
2. Napisati funkciju koja prihvata listu Stringova.
   Sve stringove spoji tako sto izmedju svaka 2 
   umetne karakter ','.

   ["ana", "voli", "milovana"] + ',' -> "ana,voli,milovana"
-}

spojiStringove :: [String] -> String
spojiStringove [] = ""
spojiStringove [s] = s
spojiStringove (h:t) = h ++ "," ++ spojiStringove t

{-
3. Napisati funkciju koja privhata listu Stringova, 
   razdvoji svaki po razmaku, te ih sve
   spoji zarezima. Koristiti map, fold i prethodne 2 funkcije.

   ["ana voli milovana", "trava je zelena"]
   -> ["ana", "voli", "milovana", "trava", "je", "zelena"]
   -> "ana,voli,milovana,trava,je,zelena"
-}

razdvojiISpoji :: [String] -> String
razdvojiISpoji ls = spojiStringove (concatMap (rastaviString ' ') ls)

{-
4. Napisati funkciju koja prihvata listu listi integer-a ( [[Int]] ).
   Potrebno je prvo kvadrirati elemente svake podliste, zatim ih sumirati.
   Na kraju potrebno je vratiti proizvod te liste.

   svastaSaListom :: Num a => [[a]] -> a

   [[1, -4, 5], [4, 4, 4], [-4, -6, -2]] -> 112896
-}

svastaSaListom :: Num a => [[a]] -> a
svastaSaListom l = product (map (sum . map (^2)) l)

{-
5. Definisati tip podataka naselje. Naselje moze da bude Selo, Varosica ili Grad.
   Sva 3 tipa naselja mogu imati broj stanovnika (integer) i povrsinu (double). 
   Selo nosi informaciju o tome da li je "zbijeno" ili "razbijeno" (string). Grad 
   ima dodatni parametar koji kaze da li sadrzi gradski bazen ili ne (boolean).
-}

data Naselje =
  Selo {stanovnici :: Int, povrsina :: Double, tip :: String} |
  Varosica {stanovnici :: Int, povrsina :: Double} |
  Grad {stanovnici :: Int, povrsina :: Double, bazen :: Bool} deriving Show

{-
6. Napisati funkciju koja iz liste Naselja izdvaja sva razbijena sela i sve
   gradove sa bazenima koji imaju vise od 150 000 stanovnika.
-}

izdvojiNaselja :: [Naselje] -> [Naselje]
izdvojiNaselja = filter odgovara
  where
    odgovara :: Naselje -> Bool
    odgovara (Selo _ _ t) = t == "razbijeno"
    odgovara (Grad s _ b) = b && s > 150000
    odgovara _ = False
