import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))
import Data.Char (isLetter)
import Data.Foldable (for_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl', transpose)
import Data.Maybe (fromJust, isNothing)

type Crates a = IntMap [a]
type Inst = (Int, Int, Int)

move :: ([a] -> [a]) -> Crates a -> Inst -> Crates a
move transform m (n, from, to) = case maybeMoved of
  Just moved -> IntMap.adjust (transform (take n moved) ++) to m'
  Nothing -> error ("there was no stack " ++ show from)
  where (maybeMoved, m') = IntMap.updateLookupWithKey (\_ stack -> Just (drop n stack)) from m

crates :: [String] -> Crates Char
crates s = IntMap.fromList (zip labels stacks)
  where labels = map read (words (last s))
        -- prepend a space to each line so I can expect each crate is four characters.
        maybeStacks = transpose (map (map maybeCrate . quads . (' ' :)) (init s))
        -- now maybeStacks have some number of Nothings, followed by Justs with their contents.
        stacks = map (map fromJust . dropWhile isNothing) maybeStacks

        quads :: [a] -> [(a, a, a, a)]
        quads [] = []
        quads (a:b:c:d:xs) = (a, b, c, d) : quads xs
        quads x = error ("bad line length " ++ show (length x))

        maybeCrate :: (Char, Char, Char, Char) -> Maybe Char
        maybeCrate (' ', '[', x, ']') | isLetter x = Just x
        maybeCrate (' ', ' ', ' ', ' ') = Nothing
        maybeCrate x = error ("bad crate format " ++ show x)

inst :: String -> Inst
inst s = case words s of
  ["move", a, "from", b, "to", c] -> (read a, read b, read c)
  _ -> error ("bad move " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let (stacks, insts) = (crates *** map inst) (splitOnOne "" (lines s))
  for_ [reverse, id] $ \f -> do
    let stacks' = foldl' (move f) stacks insts
    putStrLn (map head (IntMap.elems stacks'))
