module AdventOfCode.Split (
  splitOn
, splitOnOne
) where

import Control.Arrow (second)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s = case dropWhile (== c) s of
  [] -> []
  s' -> w : splitOn c s''
        where (w, s'') = break (== c) s'

splitOnOne :: Eq a => a -> [a] -> ([a], [a])
-- drop 1 removes c if it was found.
splitOnOne c = second (drop 1) . break (== c)
