import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  print (count (== "42") (lines s))
