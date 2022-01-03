data Stednja =
  DinarskaStednja {
    stopa :: Double,
    orocena :: Bool
  } |
  DeviznaStednja {
    stopa :: Double,
    orocena :: Bool,
    evri :: Bool
  } deriving Show

vrati :: [Stednja] -> [Stednja]
vrati = filter odabir
  where
    odabir :: Stednja -> Bool
    odabir (DinarskaStednja s _) = s > 2.9
    odabir (DeviznaStednja s o e) = (s > 1.0 && o) || (o && not e)
