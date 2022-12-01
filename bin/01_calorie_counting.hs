import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Data.List (sortBy)

main :: IO ()
main = do
  s <- readInputFile
  let elves = map (map read) (splitOn "" (lines s)) :: [[Int]]
      calories = map sum elves
  print (maximum calories)
  print (sum (take 3 (sortBy (flip compare) calories)))
