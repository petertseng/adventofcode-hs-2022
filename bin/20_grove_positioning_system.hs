import AdventOfCode (readInputFile)

{-
import Prelude hiding (seq)
--import Data.Foldable (for_)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
-}

import Control.Monad (foldM, replicateM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Foldable (for_)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

mixN :: Int -> [Int] -> [Int]
mixN rounds ints = fst $ runST $ do
  let len = length ints
  right <- newListArray (1, len) [2 .. len + 1] :: ST s (STUArray s Int Int)
  writeArray right len 1
  left <- newListArray (1, len) [0 .. len - 1] :: ST s (STUArray s Int Int)
  writeArray left 1 len

  replicateM_ rounds $
    for_ (zip ints [1 .. len]) $ \(v, i) -> do
      r <- readArray right i
      l <- readArray left i
      let rightSteps = v `mod` (len - 1)
          leftSteps = len - 1 - rightSteps
      when (rightSteps /= 0 && leftSteps /= 0) $ do
        writeArray right l r
        writeArray left r l

        (newR, newL) <- if rightSteps <= leftSteps then do
          -- for some reason, this replicate is 2x slower??
          --newR <- foldM (\pt _ -> readArray right pt) r (replicate rightSteps ())
          newR <- foldM (\pt _ -> readArray right pt) r [1 .. rightSteps]
          newL <- readArray left newR
          return (newR, newL)
        else do
          -- for some reason, this replicate is 2x slower??
          --newL <- foldM (\pt _ -> readArray left pt) l (replicate leftSteps ())
          newL <- foldM (\pt _ -> readArray left pt) l [1 .. leftSteps]
          newR <- readArray right newL
          return (newR, newL)

        writeArray left i newL
        writeArray right newL i
        writeArray right i newR
        writeArray left newR i

  foldM (\(xs, i) () -> do
      i' <- foldM (\pt () -> readArray right pt) i (replicate 1000 ())
      return (ints !! (i' - 1) : xs, i')
    ) ([], fromJust (elemIndex 0 ints) + 1) (replicate 3 ())

{-
mixNRounds :: [Int] -> [Seq Int]
mixNRounds ints = map (fmap snd) (iterate mix1Round seq)
  where seq = Seq.fromList (zip [0..] ints)

mix1Round :: Seq (Int, Int) -> Seq (Int, Int)
mix1Round seq = foldl' mix1Num seq [0 .. Seq.length seq - 1]

mix1Num :: Seq (Int, Int) -> Int -> Seq (Int, Int)
mix1Num seq i = case v of
  0 -> seq
  _ -> Seq.insertAt ((idx + v) `mod` (Seq.length seq - 1)) (ord, v) without
  where idx = fromJust (Seq.findIndexL ((== i) . fst) seq)
        (ord, v) = Seq.index seq idx
        without = Seq.deleteAt idx seq

grove :: Seq Int -> Int
grove seq = let
  z = fromJust (Seq.elemIndexL 0 seq)
  sz = Seq.length seq
  elt i = Seq.index seq ((z + i) `rem` sz)
  in elt 1000 + elt 2000 + elt 3000
-}

main :: IO ()
main = do
  s <- readInputFile
  let ints = map read (lines s)
  print (sum (mixN 1 ints))
  print (sum (mixN 10 (map (* 811589153) ints)))
{-
  print (grove (mixNRounds ints !! 1))
  let bigs = map (* 811589153) ints
      bigMixed = mixNRounds bigs !! 10
  --for_ (take 10 (mixNRounds bigs)) print
  print (grove bigMixed)
-}
