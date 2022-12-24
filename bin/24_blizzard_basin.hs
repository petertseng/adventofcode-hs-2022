import AdventOfCode (readInputFile)

import Data.Array ((!), Array, accumArray, assocs, listArray)
import Data.Bits ((.&.), (.|.), bit, clearBit, complement, shiftL, shiftR, testBit)

type Winds = (Array Int Integer, Array Int Integer, Array Int Integer, Array Int Integer)

stepTime :: Int -> Int -> Int -> Int -> Winds -> (Int, Array Int Integer) -> (Int, Array Int Integer)
stepTime y0 x0 height width (lwind, rwind, uwind, dwind) (t, poses) = (t + 1, listArray (0, height - 1) [blizzard y r | (y, r) <- assocs neighs])
  where
    blizzard :: Int -> Integer -> Integer
    blizzard y try = try .&. complement left .&. complement right .&. complement up .&. complement down
      where horizShift = (t + 1) `mod` width
            origl = lwind ! y
            left = (origl .|. (origl `shiftL` width)) `shiftR` horizShift
            origr = rwind ! y
            right = (origr `shiftL` horizShift) .|. (origr `shiftR` (width - horizShift))
            up = uwind ! ((y + t + 1) `mod` height)
            down = dwind ! ((y - t - 1) `mod` height)
    neighs :: Array Int Integer
    neighs = accumArray (.|.) 0 (0, height - 1) ((y0, bit x0) : filter ((/= 0) . snd) (map row [0 .. height - 1]))
    row :: Int -> (Int, Integer)
    row y = (y, current .|. above .|. below .|. left .|. right)
      where current = poses ! y
            above = if y == 0 then 0 else poses ! (y - 1)
            below = if y + 1 >= height then 0 else poses ! (y + 1)
            left = (current `clearBit` (width - 1)) `shiftL` 1
            right = current `shiftR` 1

enumGrid :: [[a]] -> [(a, (Int, Int))]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x c -> (c, (y, x))) [0..]

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

main :: IO ()
main = do
  s <- readInputFile
  let grid = lines s
      height = length grid - 2
      width = uniform length grid - 2
      windFor wc = accumArray (.|.) 0 (0, height - 1) [(y - 1, bit (x - 1)) | (c, (y, x)) <- enumGrid grid, c == wc]
      winds = (windFor '<', windFor '>', windFor '^', windFor 'v')

      start = (0, 0)
      goal = (height - 1, width - 1)
      time (ya, xa) (yb, xb) t0 =
        1 + fst (until (flip testBit xb . (! yb) . snd) (stepTime ya xa height width winds) (t0, accumArray (+) 0 (0, height - 1) [(ya, bit xa)]))
      t1 = time start goal 0
  print t1
  print (time start goal (time goal start t1))
