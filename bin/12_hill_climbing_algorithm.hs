import AdventOfCode (readInputFile)
import AdventOfCode.Search (bfs)

import Data.Array.Unboxed ((!), (!?), UArray, listArray)
import Data.Char (ord)
import Data.Either (rights)
import Data.List (find)
import Data.Maybe (fromJust)

type Pos = (Int, Int)
type Elevation = UArray Pos Int

neighs :: Elevation -> Pos -> [Pos]
neighs elev (y, x) = filter elevOK cands
  where cands = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
        elevOK pos = maybe False (>= (current - 1)) (elev !? pos)
        current = elev ! (y, x)

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

enumGrid :: [[a]] -> [(a, (Int, Int))]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x c -> (c, (y, x))) [0..]

grid :: String -> Elevation
grid s = listArray ((0, 0), (h - 1, w - 1)) (concat g)
  where g = map (map elev) (lines s)
        h = length g
        w = uniform length g
        elev 'S' = 0
        elev 'E' = 25
        elev c = ord c - ord 'a'

main :: IO ()
main = do
  s <- readInputFile
  let elev = grid s
      enum = enumGrid (lines s)
      coord letter = snd (fromJust (find ((== letter) . fst) enum))
      start = coord 'S'
      results = rights (bfs (neighs elev) ((== 0) . (elev !)) (coord 'E'))
  print (fst (fromJust (find ((== start) . snd) results)))
  print (fst (head results))
