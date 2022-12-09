import AdventOfCode (readInputFile)

import Data.Foldable (for_)
import Data.Containers.ListUtils (nubOrd)
import Data.List (scanl')

type Pos = (Int, Int)
newtype Dir = Dir (Int, Int) deriving (Eq, Show)

step :: [Pos] -> Dir -> [Pos]
step [] (Dir _) = error "no rope"
step ((hy, hx):tls) (Dir (dy, dx)) = h' : followAll h' tls
  where h' = (hy + dy, hx + dx)
        followAll _ [] = []
        followAll h (t:ts) = let t' = follow h t in if t' == t then t : ts else t' : followAll t' ts

follow :: Pos -> Pos -> Pos
follow (hy, hx) (ty, tx) = if far then (follow1 ty dy, follow1 tx dx) else (ty, tx)
  where dy = hy - ty
        dx = hx - tx
        far = abs dy > 1 || abs dx > 1
        follow1 t d = t + signum d

move :: String -> [Dir]
move (c:' ':n) = replicate (read n) (Dir (dir c))
  where dir 'L' = (0, -1)
        dir 'R' = (0, 1)
        dir 'U' = (-1, 0)
        dir 'D' = (1, 0)
        dir _   = error (c : " bad direction")
move s = error ("bad move " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let moves = concatMap move (lines s)

  for_ [2, 10] $ \n -> do
    let places = scanl' step (replicate n (0, 0)) moves
    print (length (nubOrd (map last places)))
