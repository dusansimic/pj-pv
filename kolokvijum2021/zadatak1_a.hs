izbaci :: String -> String
izbaci = filter cif
  where
    cif :: Char -> Bool
    cif c = c >= '0' && c <= '9'

josIzbaci :: [String] -> [String]
josIzbaci l = filter duz (map izbaci l)
  where
    duz :: String -> Bool
    duz s = ln >= 5 && ln <= 15
      where ln = length s
