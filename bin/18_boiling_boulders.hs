{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Search (bfs)
import AdventOfCode.Split (splitOn)

import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Int3 = (Int, Int, Int)

air :: Int3 -> Set Int3 -> Set Int3 -> [Int3]
air minFace closeToRock faces = map snd (rights searched)
  where searched = bfs (filter (`Set.member` closeToRock) . adj6) (`Set.member` faces) minFace

adj6 :: Int3 -> [Int3]
adj6 (x, y, z) = [(x - 1, y, z), (x + 1, y, z), (x, y - 1, z), (x, y + 1, z), (x, y, z - 1), (x, y, z + 1)]

freqMap :: Ord a => [a] -> Map a Int
freqMap = Map.fromListWith (+) . map (, 1)

rock :: String -> Int3
rock s = case splitOn ',' s of
  [a, b, c] -> (read a, read b, read c)
  _ -> error ("bad rock " ++ s)

main :: IO ()
main = do
  s <- readInputFile

  let rocks = map rock (lines s)
      rockSet = Set.fromList rocks
      faces = freqMap (concatMap (filter (`Set.notMember` rockSet) . adj6) rocks)
  print (sum (Map.elems faces))

  let closeToRock = Map.keysSet faces `Set.union` (Set.fromList (concatMap adj6 (Map.keys faces)) `Set.difference` rockSet)
      airs = Set.fromList (air (fst (Map.findMin faces)) closeToRock (Map.keysSet faces))
  print (sum [v | (k, v) <- Map.assocs faces, k `Set.member` airs])
