import AdventOfCode (readInputFile)

import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

data Dir = North | South | West | East
type Pos = (Int, Int)

propose :: Set Pos -> [Dir] -> Pos -> Maybe Pos
propose elves dirs elf = if all (`Set.notMember` elves) (adj8 elf) then Nothing else listToMaybe (mapMaybe tryDir dirs)
  where tryDir dir = if any (`Set.member` elves) (adj3 elf dir) then Nothing else Just (adj1 elf dir)

elfMove :: (Set Pos, [Dir]) -> (Set Pos, [Dir])
elfMove (elves, dirs) = (moveSuccesses, drop 1 dirs)
  where proposes = mapMaybe (\elf -> fmap (\prop -> (prop, [elf])) (propose elves (take 4 dirs) elf)) (Set.toList elves)
        combinedProposes = Map.fromListWith (++) proposes
        successfulProposes = Map.mapMaybe single combinedProposes
        moveSuccesses = Set.union (Map.keysSet successfulProposes) (elves `Set.difference` Set.fromList (Map.elems successfulProposes))

single :: [a] -> Maybe a
single [] = Nothing
single [x] = Just x
single (_:_) = Nothing

adj8 :: Pos -> [Pos]
adj8 (y, x) = [ (y - 1, x - 1), (y - 1, x), (y - 1, x + 1)
              , (y    , x - 1),             (y    , x + 1)
              , (y + 1, x - 1), (y + 1, x), (y + 1, x + 1)
              ]

adj3 :: Pos -> Dir -> [Pos]
adj3 (y, x) North = [(y - 1, x - 1), (y - 1, x), (y - 1, x + 1)]
adj3 (y, x) South = [(y + 1, x - 1), (y + 1, x), (y + 1, x + 1)]
adj3 (y, x) West = [(y - 1, x - 1), (y, x - 1), (y + 1, x - 1)]
adj3 (y, x) East = [(y - 1, x + 1), (y, x + 1), (y + 1, x + 1)]

adj1 :: Pos -> Dir -> Pos
adj1 (y, x) North = (y - 1, x)
adj1 (y, x) South = (y + 1, x)
adj1 (y, x) West = (y, x - 1)
adj1 (y, x) East = (y, x + 1)

enumGrid :: [[a]] -> [(a, (Int, Int))]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x c -> (c, (y, x))) [0..]

main :: IO ()
main = do
  s <- readInputFile
  let elves = Set.fromAscList [pos | (c, pos) <- enumGrid (lines s), c == '#']
      rounds = map fst (iterate elfMove (elves, cycle [North, South, West, East]))
      (ys, xs) = unzip (Set.toList (rounds !! 10))
      miny = minimum ys
      maxy = maximum ys
      minx = minimum xs
      maxx = maximum xs
      area = (maxy - miny + 1) * (maxx - minx + 1)
  print (area - Set.size elves)
  print (1 + fromJust (elemIndex True (zipWith (==) rounds (drop 1 rounds))))
