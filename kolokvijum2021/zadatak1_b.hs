import Data.Char

pripremi :: String -> String
pripremi s = map toLower (filter (/= ' ') s)

palindrom :: String -> Bool
palindrom s = ss == reverse ss
  where ss = pripremi s

utvrdi :: [String] -> Bool
utvrdi ss = and [ palindrom s | s <- ss ]
