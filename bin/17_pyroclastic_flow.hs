import AdventOfCode (readInputFile)

import Control.Monad (foldM)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (dropWhileEnd, foldl')
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)
type Rock = [Pos]
type Wind = Either () ()

rocks :: [Rock]
rocks = [
    [ (0, 0)
    , (0, 1)
    , (0, 2)
    , (0, 3)
    ]
  , [ (0, 1)
    , (1, 0)
    , (1, 1)
    , (1, 2)
    , (2, 1)
    ]
  , [ (0, 0)
    , (0, 1)
    , (0, 2)
    , (1, 2)
    , (2, 2)
    ]
  , [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    ]
  , [ (0, 0)
    , (0, 1)
    , (1, 0)
    , (1, 1)
    ]
  ]

rockFall :: [Wind] -> ((Int, Int), [Int])
rockFall winds = runWriter (rockFallInner Set.empty 0 (cycle winds) (length winds) 0 (cycle rocks) 0 IntMap.empty 0 0)

rockFallInner :: Set Pos -> Int -> [Wind] -> Int -> Int -> [Rock] -> Int -> IntMap Int -> Int -> Int -> Writer [Int] (Int, Int)
rockFallInner _ _ _ _ _ _ rockIdx _ expectedDiff 5 = return (expectedDiff, rockIdx)
rockFallInner _ _ _ _ _ [] _ _ _ _ = error "can't run out of rocks"
rockFallInner occ tallest winds windLen windIdx (rock:rks) rockIdx rockLock expectedDiff correctRocks = case foldM step (3 + tallest, 2, winds, windIdx) (repeat ()) of
  Right _ -> error "rock fell into the abyss"
  Left (bottom, left, winds', windIdx') ->
    let occ' = foldl' (flip Set.insert) occ (translate bottom left rock)
        tallest' = max tallest (bottom + rockHeight + 1)
        cacheKey = windIdx' * 5 + (rockIdx `rem` 5)
        (prevRockIdx, rockLock') = IntMap.insertLookupWithKey (\_ _ new -> new) cacheKey rockIdx rockLock
        (expectedDiff', correctRocks') = case prevRockIdx of
          Nothing -> (0, 0)
          Just prev | rockIdx - prev == expectedDiff -> (expectedDiff, correctRocks + 1)
          Just prev -> (rockIdx - prev, 0)
    in tell [tallest'] >> rockFallInner occ' tallest' winds' windLen windIdx' rks (rockIdx + 1) rockLock' expectedDiff' correctRocks'
  where step (bottom, left, w:ws, wi) () = if bottom == 0 || occupied (bottom - 1) left' then Left (bottom, left', ws, wi') else Right (bottom - 1, left', ws, wi')
          where left' = windBlow w bottom left
                wi' = (wi + 1) `rem` windLen
        step (_, _, [], _) () = error "can't run out of wind"
        windBlow wdir bottom left = if 0 <= left' && left' + rockWidth < 7 && not (occupied bottom left') then left' else left
          where left' = case wdir of
                  Left  () -> left - 1
                  Right () -> left + 1
        occupied bottom left = any (`Set.member` occ) (translate bottom left rock)
        translate bottom left = map (\(y, x) -> (y + bottom, x + left))
        rockWidth = maximum (map snd rock)
        rockHeight = maximum (map fst rock)


wind :: Char -> Wind
wind '<' = Left ()
wind '>' = Right ()
wind c = error (c : " isn't wind")

main :: IO ()
main = do
  s <- readInputFile
  let winds = map wind (dropWhileEnd (== '\n') s)
      -- nrocks == length allHeights
      ((cycleLen, nrocks), allHeights) = rockFall winds
      cycleHeights = drop (nrocks - 1 - cycleLen) allHeights
      endOfCycle = last cycleHeights
      startOfCycle = head cycleHeights
      heightPerCycle = endOfCycle - startOfCycle
      extrapolate t =
        let (full, partRocks) = (t - nrocks) `quotRem` cycleLen
            partHeight = cycleHeights !! partRocks - head cycleHeights
        in endOfCycle + heightPerCycle * full + partHeight
  --print (cycleLen, heightPerCycle, nrocks)
  print (if nrocks >= 2022 then allHeights !! 2021 else extrapolate 2022)
  print (extrapolate 1000000000000)
