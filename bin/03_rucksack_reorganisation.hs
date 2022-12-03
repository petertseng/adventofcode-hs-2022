import AdventOfCode (readInputFile)

import Data.Char (ord)
import Data.List (foldl1')
import qualified Data.Set as Set

score :: [String] -> Int
score = prio . single . common

common :: Ord a => [[a]] -> [a]
common = Set.toList . foldl1' Set.intersection . map Set.fromList

single :: [a] -> a
single [x] = x
single [] = error "empty not single"
single (_:_:_) = error "multiple not single"

prio :: Char -> Int
prio c | 'a' <= c && c <= 'z' = 1 + ord c - ord 'a'
prio c | 'A' <= c && c <= 'Z' = 27 + ord c - ord 'A'
prio c = error (c : " isn't a letter")

halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

trips :: [a] -> [(a, a, a)]
trips [] = []
trips (a:b:c:xs) = (a, b, c) : trips xs
trips _ = error "length not divisible by 3"

main :: IO ()
main = do
  s <- readInputFile
  let sacks = lines s
  print (sum (map (score . (\(a, b) -> [a, b]) . halves) sacks))
  print (sum (map (score . (\(a, b, c) -> [a, b, c])) (trips sacks)))
