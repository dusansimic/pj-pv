data KucniLjubimac =
  Pas {
    ime :: String,
    hrana :: Double,
    godine :: Int,
    rasa :: String
  } |
  Macka {
    ime :: String,
    hrana :: Double,
    godine :: Int,
    izlazi :: Bool
  }

vrati :: [KucniLjubimac] -> [KucniLjubimac]
vrati = filter odabir
  where
    odabir :: KucniLjubimac -> Bool
    odabir (Pas _ h _ r) = h > 1.0 && r == "zlatni retriver"
    odabir (Macka _ h g i) = (g < 5) || (h < 200 && not i)
