import AdventOfCode (readInputFile)

import Data.Array.Unboxed ((!), UArray, bounds, indices, listArray)
import Data.Char (digitToInt)
import Data.List (findIndex)

type Pos = (Int, Int)
type Trees = UArray Pos Int

countTrees :: (a -> a -> a) -> (Int -> [Int] -> a) -> Trees -> Pos -> a
countTrees op countDir trees (y, x) = left `op` right `op` up `op` down
  where up    = dir [(y', x) | y' <- [y - 1, y - 2 .. minY]]
        down  = dir [(y', x) | y' <- [y + 1 .. maxY]]
        left  = dir [(y, x') | x' <- [x - 1, x - 2 .. minX]]
        right = dir [(y, x') | x' <- [x + 1 .. maxX]]
        dir = countDir hgt . map (trees !)
        hgt = trees ! (y, x)
        ((minY, minX), (maxY, maxX)) = bounds trees

visible :: Trees -> Pos -> Bool
visible = countTrees (||) (all . (>))

score :: Trees -> Pos -> Int
score = countTrees (*) (\h xs -> maybe (length xs) succ (findIndex (>= h) xs))

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

grid :: String -> Trees
grid s = listArray ((1, 1), (h, w)) (concat g)
  where g = map (map digitToInt) (lines s)
        h = length g
        w = uniform length g

main :: IO ()
main = do
  s <- readInputFile
  let trees = grid s
  print (count (visible trees) (indices trees))
  print (maximum (map (score trees) (indices trees)))
