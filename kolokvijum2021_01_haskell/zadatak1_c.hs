import Data.Numbers.Primes (isPrime)

prosti :: [Int] -> [Int]
prosti = filter isPrime

ostatak :: [Int] -> [Int]
ostatak l = map (`rem` length l) (prosti l)
