{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Prelude hiding (round)

import Data.Array ((!), Array, bounds, listArray)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (dropWhileEnd, foldl', sortBy)

data Monkey = Monkey {
  pos :: Int
, startingItems :: [Int]
, op :: Int -> Int
, divisor :: Int
, ifTrue :: Int
, ifFalse :: Int
}

business :: [Monkey] -> (Int -> Int) -> Int -> Int
business monkeys worry nrounds = product (take 2 actives)
  where monkeysArr = listArray (0, length monkeys - 1) monkeys
        items = concatMap (\monkey -> map (, pos monkey) (startingItems monkey)) monkeys
        inspects = foldl' (IntMap.unionWith (+)) IntMap.empty (map (uncurry (itemInspects worry monkeysArr nrounds)) items)
        actives = sortBy (flip compare) (IntMap.elems inspects)

itemInspects :: (Int -> Int) -> Array Int Monkey -> Int -> Int -> Int -> IntMap Int
itemInspects worry monkeys = insp IntMap.empty IntMap.empty Nothing
  where
    insp :: IntMap Int -> IntMap Int -> Maybe (Int, Int, IntMap Int) -> Int -> Int -> Int -> IntMap Int
    -- finished
    insp inspects _ _ 0 _ _ = inspects
    -- cycle complete, skip forward
    insp inspects history (Just (cycleLen, 0, inspectsInCycle)) nrounds item itemPos = insp inspects' history (Just (cycleLen, -1, inspectsInCycle)) nrounds' item itemPos
      where completeCycle = nrounds `quot` cycleLen
            nrounds' = nrounds - completeCycle * cycleLen
            inspects' = IntMap.unionWith (+) inspects (IntMap.map (* completeCycle) inspectsInCycle)
    -- all other cases
    insp inspects history cycled nrounds item itemPos = insp inspects' history' cycled' nrounds' item' dest
      where nmonkeys = snd (bounds monkeys) + 1
            monkey = monkeys ! itemPos
            item' = worry (op monkey item)
            dest = (if item' `rem` divisor monkey == 0 then ifTrue else ifFalse) monkey
            inspects' = IntMap.insertWith (+) itemPos 1 inspects
            cacheKey = item' * nmonkeys + dest
            (cycled', history', nrounds') = case cycled of
              -- in the middle of recording a cycle
              Just (cycleLen, roundsInCycle, inspectsInCycle) -> (Just (cycleLen, roundsInCycle - dround, inspectsInCycle'), history, nrounds - dround)
                where inspectsInCycle' = IntMap.insertWith (+) itemPos 1 inspectsInCycle
                      dround = if dest < itemPos then 1 else 0
              -- still in the same round
              Nothing | dest >= itemPos -> (cycled, history, nrounds)
              -- new round, check if we found a cycle
              Nothing -> (fmap (\prev -> let len = prev - nrounds in (len, len, IntMap.empty)) (IntMap.lookup cacheKey history), IntMap.insert cacheKey nrounds history, nrounds - 1)


readMonkey :: [String] -> Monkey
readMonkey [poss, start, ops, divi, ift, iff] = Monkey {
  pos = case words poss of
    ["Monkey", n] -> read (init n)
    _ -> error ("bad monkey " ++ poss)
, startingItems = case words start of
    ("Starting":"items:":xs) -> map (read . dropWhileEnd (== ',')) xs
    _ -> error ("bad items " ++ start)
, op = case words ops of
    ["Operation:", "new", "=", "old", "*", "old"] -> \x -> x * x
    ["Operation:", "new", "=", "old", "*", n] -> \x -> x * read n
    ["Operation:", "new", "=", "old", "+", n] -> \x -> x + read n
    _ -> error ("bad op " ++ ops)
, divisor = case words divi of
    ["Test:", "divisible", "by", n] -> read n
    _ -> error ("bad divisor " ++ divi)
, ifTrue = case words ift of
    ["If", "true:", "throw", "to", "monkey", n] -> read n
    _ -> error ("bad true " ++ ift)
, ifFalse = case words iff of
    ["If", "false:", "throw", "to", "monkey", n] -> read n
    _ -> error ("bad false " ++ iff)
}
readMonkey s = error ("bad monkey " ++ show s)

main :: IO ()
main = do
  s <- readInputFile
  let monkeys = map readMonkey (splitOn "" (lines s))
  print (business monkeys (`div` 3) 20)

  let prod = product (map divisor monkeys)
  print (business monkeys (`rem` prod) 10000)
