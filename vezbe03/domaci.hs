{-
1. Kreirati novi data tip Stablo, koji moze imati elemente Nista
   i Cvor, gde Cvor sadrzi nosi neki Int, kao i levo i desno podstablo
   (koji su tipa Stablo).
-}

data Stablo = Nista
  | Cvor Int Stablo Stablo deriving Show

{-
2. Napisati funkciju "sadrzi" koja prima Int i Stablo, te pretrazuje
   da li se prosledjeni Int nalazi u stablu. Vraca Boolean vrednost.
-}

sadrzi :: Int -> Stablo -> Bool
sadrzi _ Nista = False
sadrzi br (Cvor vr s1 s2) = br == vr || sadrzi br s1 || sadrzi br s2

{-
3. Napisati funkciju uListu koja sve brojeve u stablu smesta u listu.
-}

uListu :: Stablo -> [Int]
uListu Nista = []
uListu (Cvor v s1 s2) = [v] ++ uListu s1 ++ uListu s2

{-
4. Napisati funkciju koja vraca listu svih brojeva u stablu
   koji su deljivi sa 3 ili 5.
-}

uListuDeljiviSa3Ili5 :: Stablo -> [Int]
uListuDeljiviSa3Ili5 l = filter (\x -> mod x 3 == 0 || mod x 5 == 0) (uListu l)

{-
5. Napisati funkciju preslikaj koja prima stablo i preslika ga
   "u ogledalu". (Obrne levo i desno podstablo).
-}

preslikaj :: Stablo -> Stablo
preslikaj Nista = Nista
preslikaj (Cvor v s1 s2) = Cvor v (preslikaj s2) (preslikaj s1)

{-
6. Napisati funkciju filterStablo. Ona kao parametre ocekuje funkciju f
   koja prima Int i vraca Bool, i Stablo. Rezultat funkcije filterStablo
   je lista brojeva iz stabla koji vrate True primenom funkcije f.
-}

filterStablo :: (Int -> Bool) -> Stablo -> [Int]
filterStablo _ Nista = []
filterStablo f (Cvor v s1 s2)
  | f v = [v] ++ filterStablo f s1 ++ filterStablo f s2
  | otherwise = filterStablo f s1 ++ filterStablo f s2
