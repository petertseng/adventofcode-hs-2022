{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)

import Data.List (foldl', tails)
import Data.Map (Map)
import qualified Data.Map as Map

type Dirs = Map [String] Int
type State = ([String], Bool, Dirs)

cmd :: State -> String -> State
cmd (cwd, lsing, dirs) s = case (words s, lsing) of
  (["$", "cd", tgt], _) -> (f cwd, False, dirs)
    where f = case tgt of
            "/" -> const []
            ".." -> drop 1
            t -> (t :)
  (["$", "ls"], _) -> (cwd, True, dirs)
  (["dir", _], True) -> (cwd, lsing, dirs)
  ([sizeStr, _], True) -> (cwd, lsing, Map.unionWith(+) dirs fileContrib)
    where fileContrib = Map.fromList (map (, size) (tails cwd))
          size = read sizeStr
  _ -> error ("bad cmd " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let (_, _, dirs) = foldl' cmd ([], False, Map.empty) (lines s)
      sizes = Map.elems dirs
  print (sum (filter (<= 100000) sizes))
  let need = (dirs Map.! []) - 40000000
  print (minimum (filter (>= need) sizes))
