import AdventOfCode (readInputFile)

import Data.Foldable (for_)
import Data.List (findIndex, nub, tails)
import Data.Maybe (fromJust)

windows :: Int -> [a] -> [[a]]
windows n = map (take n) . tails

main :: IO ()
main = do
  s <- readInputFile
  for_ [4, 14] $ \n -> print (fromJust (findIndex ((== n) . length . nub) (windows n s)) + n)
