{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Prelude hiding (round)

import Data.Array ((!), Array, listArray)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (dropWhileEnd, sortBy, unfoldr)

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
        inspects = freqMap (concatMap (uncurry (poses worry monkeysArr nrounds)) items)
        actives = sortBy (flip compare) (IntMap.elems inspects)

poses :: (Int -> Int) -> Array Int Monkey -> Int -> Int -> Int -> [Int]
poses worry monkeys nrounds item itemPos = unfoldr (turn worry monkeys) (nrounds, item, itemPos)

turn :: (Int -> Int) -> Array Int Monkey -> (Int, Int, Int) -> Maybe (Int, (Int, Int, Int))
turn _ _ (0, _, _) = Nothing
turn worry monkeys (nrounds, item, itemPos) = Just (itemPos, (nrounds', item', dest))
  where monkey = monkeys ! itemPos
        item' = worry (op monkey item)
        dest = (if item' `rem` divisor monkey == 0 then ifTrue else ifFalse) monkey
        nrounds' = nrounds - if dest < itemPos then 1 else 0

freqMap :: [Int] -> IntMap Int
freqMap = IntMap.fromListWith (+) . map (, 1)

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
