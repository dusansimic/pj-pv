mapiraj :: [[Int]] -> [Int]
mapiraj = map (\x -> if length x >= 5 then sum x else product x)

sumiraj :: [[Int]] -> Int
sumiraj l = sum (map (\x -> if mod x 3 == 0 then x*x else x) (mapiraj l))
