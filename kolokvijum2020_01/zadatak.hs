{-
Zadatak 1 (10p)

Potrebno je napisati funkciju koja prihvata listu listi integer-a [[Int]]. Svakom elementu
na parnom mestu u svakoj podlisti promeni znak. (Indeksiranje pocinje od 0)

Zatim, napisati funkciju koja prima [[Int]] i nad njom primeni prethodnu funkciju.
Nakon sumira ​samo parne​ elemente svake podliste ​(map i fold). ​Posle toga izbacuju
se sve sume koje su manje od 99 ​(filter)​.
-}

promeniZnak :: [[Int]] -> [[Int]]
promeniZnak = map (\l -> [ if even i then -v else v | (i, v) <- zip [0..] l ])

sumiraj :: [[Int]] -> [Int]
sumiraj l = filter (> 99) (map (sum . filter even) (promeniZnak l))
-- sumiraj l = filter (> 99) (map (foldr addIfEven 0) (promeniZnak l))
--   where
--     addIfEven :: Int -> Int -> Int
--     addIfEven x a = if even x then x + a else a

{-
Zadatak 2 (8p)

Definisati tip podataka ​StambeniObjekat​. Stambeni objekat opisan je svojom
kvadraturom (double) i kapacitetom (int - broj ljudi koji moze da zivi u tom objektu).
StambeniObjekat ​moze biti ​SeoskaKuca, GradskaKuca ​i ​Zgrada​. SeoskaKuca kao
dodatnu osobinu ima povrsinu dvorista u arima (double), a zgrada ima broj spratova
(int).

Napisati funkciju koja iz liste stambenih objekata izdvaja one ​SeoskeKuce​ koje imaju
dvoriste od barem 5 ari, sve gradske kuce, kao i zgrade koje su visoke barem 7
spratova. Na kraju potrebno je vratiti ukupni kapacitet svih izdvojenih objekata.
-}

data StambeniObjekat =
  SeoskaKuca {
    kvadratura :: Double,
    kapacitet :: Int,
    dvoriste :: Double
  } |
  Zgrada {
    kvadratura :: Double,
    kapacitet :: Int,
    spratovi :: Int
  } |
  GradskaKuca {
    kvadratura :: Double,
    kapacitet :: Int
  }

izdvoji :: [StambeniObjekat] -> [StambeniObjekat]
izdvoji = filter odgovara
  where
    odgovara :: StambeniObjekat -> Bool
    odgovara (SeoskaKuca _ _ dvor) = dvor >= 5
    odgovara (GradskaKuca _ _) = True
    odgovara (Zgrada _ _ spr) = spr >= 7

ukupniKapacitet :: [StambeniObjekat] -> Int
ukupniKapacitet l = sum (map kapacitet l)

{-
Zadatak 3 (7p)

Potrebno je napisati funkciju koja će da izvrši klasterizovanje (grupisanje) liste tačaka L
u K klastera (grupa) pomoću K-Means algoritma. Ovaj algoritam se izvodi u više
iteracija. Počev od nekih K centralnih tačaka (centri klastera), algoritam izvodi sledeće
korake:

- svakoj tački iz L pronađe najbližu od centralnih tačaka. Tačka pripada onom
klasteru čijem centru je najbliža.
- nakon što je svakoj tački iz L dodeljen klaster, računaju se nove centralne tačke.

Novi centar svakog klastera računa se na osnovu svih tačaka iz L koji pripadaju
tom klasteru. Centar klastera​ i​ u ​narednoj i​ teraciji računa se kao suma svih tačaka klastera ​i
​u trenutnoj iteraciji podeljena sa brojem tačaka koji pripadaju tom klasteru.

Algoritam se završava kada između 2 iteracije ne dođe do promene u klasterima. Za
početne centre klastera uzeti K nasumičnih tačaka iz liste tačaka L.
Glavna funkcija očekuje listu tačaka L (tačka = par Double-ova), kao i neko K koje
predstavlja broj klastera na koji je potrebno podeliti listu tačaka L.Funkcija vraća listu
parova oblika:
( (Double, Double) , Int ) koji predstavljaju (tacku, broj klastera)
-}

type Tacka = (Double, Double)

klasterizuj :: [Tacka] -> Int -> [(Tacka, Int)]
klasterizuj ts k = klasterovane' ts (take k ts)


distanca :: Tacka -> Tacka -> Double
distanca (x1, y1) (x2, y2) = sqrt (abs (x1 - x2) * abs (x1 - x2) + abs (y1 - y2) * abs (y1 - y2))

najbliza :: [Tacka] -> Tacka -> Tacka
najbliza (h:t) = najbliza' h t
  where
    najbliza' :: Tacka -> [Tacka] -> Tacka -> Tacka
    najbliza' n [] _ = n
    najbliza' n (h:t) p
      | distanca n p < distanca h p = najbliza' h t p
      | otherwise = najbliza' n t p

sumiraneTacke :: [Tacka] -> Tacka
sumiraneTacke = foldr (\(x, y) (xa, ya) -> (x + xa, y + ya)) (0, 0)

noviCentar :: [Tacka] -> Tacka
noviCentar l = podeli (sumiraneTacke l) (length l)
  where
    podeli :: Tacka -> Int -> Tacka
    podeli (x, y) n = (x / fromIntegral n, y / fromIntegral n)

noviCentri :: [[Tacka]] -> [Tacka]
noviCentri = map noviCentar

grupisaneTacke :: [(Tacka, Int)] -> Int -> [[Tacka]]
grupisaneTacke l n = map (map fst . samoK) [1..n]
  where
    samoK :: Int -> [(Tacka, Int)]
    samoK k = filter (\x -> snd x == k) l

indeks :: Eq a => [a] -> a -> Int
indeks = pom 0
  where
    pom :: Eq a => Int -> [a] -> a -> Int
    pom i [] _ = -1
    pom i (h:t) a
      | h == a = i
      | otherwise = pom (i + 1) t a

klasterovane' :: [Tacka] -> [Tacka] -> [(Tacka, Int)]
klasterovane' ts cnt
  | cnt == new_cnt = finalizuj clts cnt
  | otherwise = klasterovane' ts new_cnt
  where
    clts = klasterovane'' ts cnt
    new_cnt = noviCentri clts

klasterovane'' :: [Tacka] -> [Tacka] -> [[Tacka]]
klasterovane'' ts cs = grupisaneTacke parovi (length cs)
  where
    parovi = [ (t, indeks cs (najbliza cs t)) | t <- ts ]

finalizuj :: [[Tacka]] -> [Tacka] -> [(Tacka, Int)]
finalizuj ts cs = concat [ [ (t, indeks cs c) | t <- ts ] | (ts, c) <- zip ts cs]
