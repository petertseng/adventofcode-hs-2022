import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))

type Interval = (Int, Int)

contain :: (Interval, Interval) -> Bool
contain ((a, b), (c, d)) = a >= c && b <= d || c >= a && d <= b

intersects :: (Interval, Interval) -> Bool
intersects ((a, b), (c, d)) = a <= d && c <= b

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

parse :: String -> (Interval, Interval)
parse = (parse' *** parse') . splitOnOne ','
  where parse' = (read *** read) . splitOnOne '-'

main :: IO ()
main = do
  s <- readInputFile
  let intervals = map parse (lines s)
  print (count contain intervals)
  print (count intersects intervals)
