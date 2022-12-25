{-# LANGUAGE BinaryLiterals #-}

import AdventOfCode (readInputFile)

import Data.Array ((!), Array, listArray)
import Data.Bits ((.&.), (.|.), bit, shiftL, shiftR)
import Data.List (elemIndex, find, mapAccumL)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

data Dir = North | South | West | East deriving (Eq)
type Pos = (Int, Int)

neighs :: Array Int (Bool, [Dir])
neighs = listArray (0, 511) [(n .&. 0b111101111 == 0, [dir | dir <- [North, South, East, West], n .&. dirMask dir == 0]) | n <- [0 .. 511 :: Int]]
  where dirMask North = 0b001001001
        dirMask South = 0b100100100
        dirMask East  = 0b111000000
        dirMask West  = 0b000000111

neighsByDir :: Array Int (Maybe Dir)
neighsByDir = listArray (0, 2047) [firstDir (neighs ! n) (take 4 (drop i (cycle [North, South, West, East]))) | n <- [0 .. 511], i <- [0 .. 3]]
  where firstDir (True, _) _ = Nothing
        firstDir (False, freeDirs) wantDirs = find (`elem` freeDirs) wantDirs

propose :: Set Pos -> Int -> (Int, Int, Int) -> Pos -> ((Int, Int, Int), Maybe (Pos, [Pos]))
propose elves t (prevY, _, _) pos@(y, x) | y /= prevY = propose elves t (y, x - 3, 0) pos
propose elves t (_, prevX, neigh) pos@(y, x) =
  let dx = x - prevX
      neigh0 = neigh `shiftR` (dx * 3)
      neigh1 = if dx > 2 then neigh0 .|. col elves y (x - 1) 0 else neigh0
      neigh2 = if dx > 1 then neigh1 .|. col elves y x 3 else neigh1
      neigh3 = neigh2 .|. col elves y (x + 1) 6
  in ((y, x, neigh3), fmap (\dir -> (step dir pos, [pos])) (neighsByDir ! ((neigh3 `shiftL` 2) .|. (t .&. 0b11))))

col :: Set Pos -> Int -> Int -> Int -> Int
col elves y x offset = (if (y - 1, x) `Set.member` elves then bit offset       else 0)
                   .|. (if (y    , x) `Set.member` elves then bit (offset + 1) else 0)
                   .|. (if (y + 1, x) `Set.member` elves then bit (offset + 2) else 0)

elfMove :: (Set Pos, [Dir], Int) -> (Set Pos, [Dir], Int)
elfMove (elves, dirs, t) = (moveSuccesses, drop 1 dirs, t + 1)
  where (_, proposes) = mapAccumL (propose elves t) (maxBound, maxBound, 0) (Set.toList elves)
        combinedProposes = Map.fromListWith (++) (catMaybes proposes)
        successfulProposes = Map.mapMaybe single combinedProposes
        moveSuccesses = Set.union (Map.keysSet successfulProposes) (elves `Set.difference` Set.fromList (Map.elems successfulProposes))

single :: [a] -> Maybe a
single [] = Nothing
single [x] = Just x
single (_:_) = Nothing

step :: Dir -> Pos -> Pos
step North (y, x) = (y - 1, x)
step South (y, x) = (y + 1, x)
step West (y, x) = (y, x - 1)
step East (y, x) = (y, x + 1)

enumGrid :: [[a]] -> [(a, (Int, Int))]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x c -> (c, (y, x))) [0..]

main :: IO ()
main = do
  s <- readInputFile
  let elves = Set.fromAscList [pos | (c, pos) <- enumGrid (lines s), c == '#']
      rounds = map (\(a, _, _) -> a) (iterate elfMove (elves, cycle [North, South, West, East], 0))
      (ys, xs) = unzip (Set.toList (rounds !! 10))
      miny = minimum ys
      maxy = maximum ys
      minx = minimum xs
      maxx = maximum xs
      area = (maxy - miny + 1) * (maxx - minx + 1)
  print (area - Set.size elves)
  print (1 + fromJust (elemIndex True (zipWith (==) rounds (drop 1 rounds))))
