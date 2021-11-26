validna :: [Int] -> Int -> Bool
validna l m = not $ any (\(r, c) -> c == m || (abs (r - k) == abs (c - m))) (zip [1..] l)
  where k = length l + 1

kraljice :: Int -> [[Int]]
kraljice n = concatMap (\m -> kraljice' [m] n) [1..n]

kraljice' :: [Int] -> Int -> [[Int]]
kraljice' l n
  | length l == n = [l]
  | otherwise = concatMap (\m -> kraljice' (l ++ [m]) n) (filter (validna l) [1..n])
