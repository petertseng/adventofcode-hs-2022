{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))
import Control.Monad (unless, when)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Foldable (for_)
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Data.Tuple (swap)

type Pos = (Int, Int)

sand :: Int -> [Pos] -> (Int, Int)
sand abyss rocks = runST $ do
  blocked <- newArray ((0, 500 - abyss - 1), (abyss + 1, 500 + abyss + 1)) False :: ST s (STUArray s Pos Bool)
  for_ rocks (\r -> writeArray blocked r True)

  rested <- newSTRef 0
  firstAbyss <- newSTRef (maxBound :: Int)

  let sand' y x = do
        wasBlock <- readArray blocked (y, x)
        unless wasBlock $ do
          when (y >= abyss) (readSTRef rested >>= modifySTRef' firstAbyss . min)
          when (y <= abyss) $ do
            sand' (y + 1) x
            sand' (y + 1) (x - 1)
            sand' (y + 1) (x + 1)
            modifySTRef' rested succ
            writeArray blocked (y, x) True
  sand' 0 500

  fa <- readSTRef firstAbyss
  re <- readSTRef rested
  return (fa, re)

rockPath :: String -> [Pos]
rockPath s = map coord (pairs ("->" : words s))

expandPairs :: [Pos] -> [Pos]
expandPairs xs = concat (zipWith between xs (drop 1 xs))
  where between (y1, x1) (y2, x2) | x1 == x2 = [(y, x1) | y <- [min y1 y2 .. max y1 y2]]
        between (y1, x1) (y2, x2) | y1 == y2 = [(y1, x) | x <- [min x1 x2 .. max x1 x2]]
        between a b = error ("don't share an axis " ++ show a ++ show b)

pairs :: [String] -> [String]
pairs [] = []
pairs [x] = error (x ++ " isn't a pair")
pairs ("->":x:xs) = x : pairs xs
pairs (x:_) = error (x ++ " isn't an arrow")

coord :: String -> Pos
coord = (read *** read) . swap . splitOnOne ','

main :: IO ()
main = do
  s <- readInputFile
  let rockPaths = map rockPath (lines s)
      abyss = maximum (map fst (concat rockPaths)) + 1
      rocks = concatMap expandPairs rockPaths
      (abyssGrains, totalGrains) = sand abyss rocks
  print abyssGrains
  print totalGrains
