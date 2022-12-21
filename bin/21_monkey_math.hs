import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow (second)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio ((%), Ratio, denominator, numerator)

-- Some of my numerators/denominators exceed Int range, so I actually need Integer here.
type Rat = Ratio Integer
data Monkey = Const Rat | Op String String (Rat -> Rat -> Rat)

math :: Map String Monkey -> Map String Rat
math monkeys = cache
  where cache = Map.map eval monkeys
        eval (Const n) = n
        eval (Op a b op) = (cache Map.! a) `op` (cache Map.! b)

nextGuess :: (Rat -> Rat) -> Rat -> (Rat, Rat, Rat) -> (Rat, Rat, Rat)
nextGuess result checkVal (prevGuess, prevResult, curGuess) = (curGuess, newResult, newGuess)
  where newResult = result curGuess
        deltaResult = newResult - prevResult
        remainingDiff = checkVal - newResult
        newGuess = curGuess + ((curGuess - prevGuess) * remainingDiff / deltaResult)

monkey :: String -> (String, Monkey)
monkey = second job . splitOnOne ':'

job :: String -> Monkey
job s = case words s of
  [n] -> Const (read n % 1)
  [a, op, b] -> Op a b (case op of
    "+" -> (+)
    "-" -> (-)
    "*" -> (*)
    "/" -> (/)
    _ -> error ("bad op " ++ op)
    )
  _ -> error ("bad monkey " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let monkeys = Map.fromList (map monkey (lines s))
      mathed = math monkeys
      mustBeInt r = if denominator r == 1 then numerator r else error (show r ++ " not integer")
  print (mustBeInt (mathed Map.! "root"))

  let (rootl, rootr) = case monkeys Map.! "root" of
        Const _ -> error "root is a constant"
        Op a b _ -> (a, b)
      origl = mathed Map.! rootl
      origr = mathed Map.! rootr
      withHuman n = math (Map.insert "humn" (Const n) monkeys)
      withHuman0 = withHuman 0
      l' = withHuman0 Map.! rootl
      r' = withHuman0 Map.! rootr
      (changingMonkey, changingVal, checkVal) = case (origl == l', origr == r') of
        (True, False) -> (rootr, r', origl)
        (False, True) -> (rootl, l', origr)
        (True, True) -> error "neither changed"
        (False, False) -> error "both changed"
      guess v = withHuman v Map.! changingMonkey
      (answer, _, _) = until (\(a, _, c) -> a == c) (nextGuess guess checkVal) (0, changingVal, 100)
  print (mustBeInt answer)
