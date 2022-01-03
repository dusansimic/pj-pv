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
