import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Arrow ((***), first, second)
import Data.Either (either, fromLeft, fromRight, lefts, partitionEithers, rights)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.List (dropWhileEnd, elemIndex, find, findIndex, findIndices, foldl', group, groupBy, inits, intercalate, intersperse, mapAccumL, maximumBy, minimumBy, partition, permutations, scanl', sort, sortOn, sortBy, subsequences, tails, transpose, unfoldr)
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe, maybe)
import Data.Ord (comparing)
import Debug.Trace
import Text.Printf (printf)
-- reminder: `fromJust (find p (iterate f x))` can instead be `until p f x`

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  print (count (== "42") (lines s))
