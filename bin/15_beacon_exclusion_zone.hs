import AdventOfCode (readInputFile)

import Data.Containers.ListUtils (nubInt)
import qualified Data.IntSet as IntSet
import Data.List (sort)
import Data.Maybe (mapMaybe)

-- TODO: Range and Beacon alias, maybe don't let them.
type Range = (Int, Int)
type Beacon = (Int, Int)
type Scanner = (Int, Int, Int)

xsAtY :: Int -> Scanner -> Range
xsAtY targy (y, x, r) = (x - remainDist, x + remainDist)
  where remainDist = r - abs (y - targy)

merge :: [Range] -> [Range]
merge = mergeSorted . sort

mergeSorted :: [Range] -> [Range]
mergeSorted [] = []
mergeSorted ((min1, max1):(min2, max2):xs) | succ max1 >= min2 = mergeSorted ((min1, max max1 max2) : xs)
mergeSorted (x:xs) = x : mergeSorted xs

rangeSize :: Range -> Int
rangeSize (l, r) | l > r = 0
rangeSize (l, r) = r - l + 1

plusMinusR :: Int -> [Scanner] -> [Int]
plusMinusR yMult scanners = IntSet.toList (IntSet.fromList plusR `IntSet.intersection` IntSet.fromList minusR)
  where plusR = map (\(y, x, r) -> x + y * yMult + r + 1) scanners
        minusR = map (\(y, x, r) -> x + y * yMult - r - 1) scanners

xy :: Int -> Int -> Maybe (Int, Int)
xy xplusy xminusy = if odd twiceX then Nothing else Just (xplusy - x, x)
  where twiceX = xplusy + xminusy
        x = twiceX `quot` 2

one :: [a] -> a
one [x] = x
one [] = error "none"
one (_:_) = error "too many"

sensor :: String -> (Scanner, Beacon)
sensor s = case words s of
  ["Sensor", "at", 'x':'=':x1s, 'y':'=':y1s, "closest", "beacon", "is", "at", 'x':'=':x2s, 'y':'=':y2s] -> ((y1, x1, r), (y2, x2))
    where y1 = read (init y1s)
          x1 = read (init x1s)
          y2 = read y2s
          x2 = read (init x2s)
          r = abs (y1 - y2) + abs (x1 - x2)
  _ -> error ("bad sensor " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let (scanners, beacons) = unzip (map sensor (lines s))
      sample = length beacons == 14
      targy = if sample then 10 else 2000000
      beaconsThere = length (nubInt (map snd (filter ((== targy) . fst) beacons)))
  print (sum (map rangeSize (merge (map (xsAtY targy) scanners))) - beaconsThere)

  let pluses = plusMinusR 1 scanners
      minuses = plusMinusR (-1) scanners
      pairs = [(p, m) | p <- pluses, m <- minuses]
      maxCoord = targy * 2
      notDetect (y1, x1) (y2, x2, r) = abs (y1 - y2) + abs (x1 - x2) > r
      ok (y, x) = 0 <= y && y <= maxCoord && 0 <= x && x <= maxCoord && all (notDetect (y, x)) scanners
      cands = mapMaybe (uncurry xy) pairs
      (yy, xx) = one (filter ok cands)
  print (xx * 4000000 + yy)
