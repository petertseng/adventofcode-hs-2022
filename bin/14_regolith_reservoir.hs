import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))
import Control.Monad (when)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Semigroup (Min(Min))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

type Pos = (Int, Int)

sand :: Int -> Set Pos -> (Int, Int)
sand abyss rocks = (abyssGrain - Set.size rocks, Set.size total - Set.size rocks)
  where (total, Min abyssGrain) = runWriter (sand' 0 500 rocks)
        sand' :: Int -> Int -> Set Pos -> Writer (Min Int) (Set Pos)
        sand' y x blocked | (y, x) `Set.member` blocked = return blocked
        sand' y _ blocked | y > abyss = return blocked
        sand' y x blocked = do
          when (y >= abyss) $ tell (Min (Set.size blocked))
          fmap (Set.insert (y, x)) (sand' (y + 1) x blocked >>= sand' (y + 1) (x - 1) >>= sand' (y + 1) (x + 1))

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
      rocks = Set.fromList (concatMap expandPairs rockPaths)
      (abyssGrains, totalGrains) = sand abyss rocks
  print abyssGrains
  print totalGrains
