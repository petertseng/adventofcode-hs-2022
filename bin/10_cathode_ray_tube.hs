import AdventOfCode (readInputFile)

import Control.Arrow (second)
import Data.Foldable (for_)
import Data.List (mapAccumL)

data Inst = Noop | AddX Int

run :: Int -> Inst -> (Int, [Int])
run x Noop = (x, [x])
run x (AddX n) = (x + n, [x, x + n])

slices :: Int -> [a] -> [[a]]
slices _ [] = []
slices n xs = let (a, b) = splitAt n xs in a : slices n b

inst :: String -> Inst
inst s = case words s of
  ["noop"] -> Noop
  ["addx", n] -> AddX (read n)
  _ -> error ("bad inst " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let insts = map inst (lines s)
      (_, sig) = second concat (mapAccumL run 1 insts)
  print (sum [t * (sig !! (t - 2)) | t <- [20, 60 .. 220]])

  for_ (take 6 (slices 40 (1 : sig))) (putStrLn . zipWith (\a b -> if abs (a - b) <= 1 then '#' else ' ') [0..])
