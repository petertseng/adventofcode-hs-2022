import AdventOfCode (readInputFile)

import Control.Monad (foldM, unless)
import Data.Array ((!), Array, assocs, bounds, listArray)
import Data.Array.MArray (modifyArray, newArray)
import Data.Array.ST (runSTArray)
import Data.Bits ((.&.), (.|.), complement, popCount, shiftR, shiftL)
import Data.Foldable (for_)
import Data.List (find, foldl', unfoldr)
import Data.Maybe (fromMaybe)

data Dir = North | South | West | East

step :: (Array Int Integer, [Dir]) -> Maybe (Int, (Array Int Integer, [Dir]))
step (grid, dirs) = (\grid' -> if bounds grid' == (0, 0) then Nothing else Just (nonzeroArea grid', (grid', drop 1 dirs))) $ runSTArray $ do
  let (minY, maxY) = bounds grid
      needPadBefore = grid ! minY /= 0
      needPadAfter = grid ! maxY /= 0
      minY' = minY - if needPadBefore then 1 else 0
      maxY' = maxY + if needPadAfter then 1 else 0
  newGrid <- newArray (minY', maxY') 0
  let setBits _ 0 = return ()
      setBits y bits = modifyArray newGrid y (.|. bits)

  (intoNew, intoLast, extendingEast, anyMoved) <- foldM (\(prevSouth, prevPrevSouth, extEast, anyMoved') (y, row) -> do
      let north = if y == minY then 0 else grid ! (y - 1)
          northwest = north `shiftR` 1
          northeast = north `shiftL` 1
          norths = north .|. northwest .|. northeast

          south = if y == maxY then 0 else grid ! (y + 1)
          southwest = south `shiftR` 1
          southeast = south `shiftL` 1
          souths = south .|. southwest .|. southeast

          west = row `shiftR` 1
          east = row `shiftL` 1

          lonely = row .&. complement west .&. complement east .&. complement norths .&. complement souths

          (proposeNorth, proposeSouth, proposeWest, proposeEast, canMove) = foldl' proposeDir (undefined, undefined, undefined, undefined, row .&. complement lonely) (take 4 dirs)
          proposeDir (_, ps, pw, pe, cm) North = let pn = cm .&. complement norths in (pn, ps, pw, pe, cm .&. complement pn)
          proposeDir (pn, _, pw, pe, cm) South = let ps = cm .&. complement souths in (pn, ps, pw, pe, cm .&. complement ps)
          proposeDir (pn, ps, _, pe, cm) West = let pw = cm .&. complement northwest .&. complement southwest .&. complement west in (pn, ps, pw, pe, cm .&. complement pw)
          proposeDir (pn, ps, pw, _, cm) East = let pe = cm .&. complement northeast .&. complement southeast .&. complement east in (pn, ps, pw, pe, cm .&. complement pe)

          northOK = proposeNorth .&. complement prevPrevSouth
          prevPrevSouthOK = prevPrevSouth .&. complement proposeNorth
      setBits (y - 1) (northOK .|. prevPrevSouthOK)

      let northSouthConflict = proposeNorth .&. prevPrevSouth
      setBits (y - 2) northSouthConflict

      let noMove = lonely .|. canMove
          eastOK = proposeEast .&. complement (proposeWest `shiftL` 2)
          eastConflict = proposeEast .&. (proposeWest `shiftL` 2)
          westOK = proposeWest .&. complement (proposeEast `shiftR` 2)
          westConflict = proposeWest .&. (proposeEast `shiftR` 2)
      setBits y (noMove .|. eastConflict .|. westConflict .|. eastOK `shiftR` 1 .|. westOK `shiftL` 1 .|. northSouthConflict)

      return (proposeSouth, prevSouth, if odd eastOK then y : extEast else extEast, anyMoved' || eastOK /= 0 || westOK /= 0 || northOK /= 0 || prevPrevSouthOK /= 0)
    ) (0, 0, [], False) (assocs grid)
  setBits maxY intoLast
  setBits (maxY + 1) intoNew
  unless (null extendingEast) $ do
    for_ [minY' .. maxY'] $ \y -> modifyArray newGrid y (`shiftL` 1)
    for_ extendingEast $ \y -> setBits y 1
  -- Uh okay I really need to learn a way to return a Maybe out of runSTArray
  if anyMoved || intoLast /= 0 || intoNew /= 0 then return newGrid else newArray (0, 0) 0

nonzeroArea :: Array Int Integer -> Int
nonzeroArea grid = height * width
  where width = maximum (fmap bitSize grid)
        height = maxY' - minY' + 1
        (minY, maxY) = bounds grid
        minY' = fromMaybe maxY (find ((/= 0) . (grid !)) [minY .. maxY])
        maxY' = fromMaybe minY (find ((/= 0) . (grid !)) [maxY, maxY - 1 .. minY])
        -- hmmm, Integer doesn't have finiteBitSize, so I have to do it myself?
        bitSize 0 = 0
        bitSize n | n >= 512 = 10 + bitSize (n `shiftR` 10)
        bitSize n = 1 + bitSize (n `shiftR` 1)

main :: IO ()
main = do
  s <- readInputFile
  let elves = map (foldl' elf 0) (lines s)
      elf row '.' = row `shiftL` 1
      elf row '#' = row `shiftL` 1 .|. 1
      elf _ c = error (c : " is not an elf")
      numElves = sum (map popCount elves)
      elvesArr = listArray (0, length elves - 1) elves
      dirs = [North, South, West, East]
  case drop 9 (unfoldr step (elvesArr, cycle dirs)) of
    [] -> print "ended before 10th iteration"
    (area:_) -> print (area - numElves)
  print (length (unfoldr step (elvesArr, cycle dirs)) + 1)
