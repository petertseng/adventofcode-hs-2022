import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Prelude hiding (Left, Right)

import Data.Char (isDigit)
import Data.Foldable (for_)
import Data.List (elemIndex, groupBy, foldl')
--import Data.List (scanl')
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

data Move = TurnLeft | TurnRight | Move Int
type Pos = (Int, Int)
data Dir = Right | Down | Left | Up deriving (Enum, Show)

step :: (Dir -> Pos -> (Pos, Dir)) -> Set Pos -> (Pos, Dir) -> Move -> (Pos, Dir)
step _ _ (p, d) TurnLeft = (p, toEnum ((fromEnum d - 1) `mod` 4))
step _ _ (p, d) TurnRight = (p, toEnum ((fromEnum d + 1) `mod` 4))
step _ _ pd (Move 0) = pd
step adj wall pd@((y, x), dir) (Move n) = let (pos', dir') = adj dir (y + dy, x + dx) in
  if pos' `Set.member` wall then pd else step adj wall (pos', dir') (Move (n - 1))
  where (dy, dx) = dydx dir

dydx :: Dir -> (Int, Int)
dydx Right = (0, 1)
dydx Down = (1, 0)
dydx Left = (0, -1)
dydx Up = (-1, 0)

dontChangeDirection :: (Dir -> Pos -> Pos) -> Dir -> Pos -> (Pos, Dir)
dontChangeDirection f d p = (f d p, d)

wrapBig :: Dir -> Pos -> Pos
wrapBig Left (y, 49) | y `between` (0, 50) = (y, 149)
wrapBig Right (y, 150) | y `between` (0, 50) = (y, 50)
wrapBig Left (y, 49) | y `between` (50, 100) = (y, 99)
wrapBig Right (y, 100) | y `between` (50, 100) = (y, 50)
wrapBig Left (y, -1) | y `between` (100, 150) = (y, 99)
wrapBig Right (y, 100) | y `between` (100, 150) = (y, 0)
wrapBig Left (y, -1) | y `between` (150, 200) = (y, 49)
wrapBig Right (y, 50) | y `between` (150, 200) = (y, 0)
wrapBig Up (99, x) | x `between` (0, 50) = (199, x)
wrapBig Down (200, x) | x `between` (0, 50) = (100, x)
wrapBig Up (-1, x) | x `between` (50, 100) = (149, x)
wrapBig Down (150, x) | x `between` (50, 100) = (0, x)
wrapBig Up (-1, x) | x `between` (100, 150) = (49, x)
wrapBig Down (50, x) | x `between` (100, 150) = (0, x)
wrapBig _ p = p

cubeBig :: Dir -> Pos -> (Pos, Dir)
cubeBig Left (y, 49) | y `between` (0, 50) = ((149 - y, 0), Right)
cubeBig Right (y, 150) | y `between` (0, 50) = ((149 - y, 99), Left)
cubeBig Left (y, 49) | y `between` (50, 100) = ((100, y - 50), Down)
cubeBig Right (y, 100) | y `between` (50, 100) = ((49, 100 + (y - 50)), Up)
cubeBig Left (y, -1) | y `between` (100, 150) = ((49 - (y - 100), 50), Right)
cubeBig Right (y, 100) | y `between` (100, 150) = ((49 - (y - 100), 149), Left)
cubeBig Left (y, -1) | y `between` (150, 200) = ((0, 50 + (y - 150)), Down)
cubeBig Right (y, 50) | y `between` (150, 200) = ((149, 50 + (y - 150)), Up)
cubeBig Up (99, x) | x `between` (0, 50) = ((50 + x, 50), Right)
cubeBig Down (200, x) | x `between` (0, 50) = ((0, 100 + x), Down)
cubeBig Up (-1, x) | x `between` (50, 100) = ((150 + (x - 50), 0), Right)
cubeBig Down (150, x) | x `between` (50, 100) = ((150 + (x - 50), 49), Left)
cubeBig Up (-1, x) | x `between` (100, 150) = ((199, x - 100), Up)
cubeBig Down (50, x) | x `between` (100, 150) = ((50 + (x - 100), 99), Left)
cubeBig d (y, x) = ((y, x), d)

wrapSmall :: Dir -> Pos -> Pos
wrapSmall Left (y, 7) | y `between` (0, 4) = (y, 11)
wrapSmall Right (y, 12) | y `between` (0, 4) = (y, 8)
wrapSmall Left (y, -1) | y `between` (4, 8) = (y, 11)
wrapSmall Right (y, 12) | y `between` (4, 8) = (y, 0)
wrapSmall Left (y, 7) | y `between` (8, 12) = (y, 15)
wrapSmall Right (y, 16) | y `between` (8, 12) = (y, 8)
wrapSmall Up (3, x) | x `between` (0, 8) = (7, x)
wrapSmall Down (8, x) | x `between` (0, 8) = (4, x)
wrapSmall Up (-1, x) | x `between` (8, 12) = (11, x)
wrapSmall Down (12, x) | x `between` (8, 12) = (0, x)
wrapSmall Up (7, x) | x `between` (12, 16) = (11, x)
wrapSmall Down (12, x) | x `between` (12, 16) = (8, x)
wrapSmall _ p = p

cubeSmall :: Dir -> Pos -> (Pos, Dir)
cubeSmall Left (y, 7) | y `between` (0, 4) = ((4, 4 + y), Down)
cubeSmall Right (y, 12) | y `between` (0, 4) = ((11 - y, 15), Left)
cubeSmall Left (y, -1) | y `between` (4, 8) = ((11, 15 - (y - 4)), Up)
cubeSmall Right (y, 12) | y `between` (4, 8) = ((8, 15 - (y - 4)), Down)
cubeSmall Left (y, 7) | y `between` (8, 12) = ((7, 7 - (y - 8)), Up)
cubeSmall Right (y, 16) | y `between` (8, 12) = ((11 - y, 11), Left)
cubeSmall Up (3, x) | x `between` (0, 4) = ((0, 11 - x), Down)
cubeSmall Down (8, x) | x `between` (0, 4) = ((11, 11 - x), Up)
cubeSmall Up (3, x) | x `between` (4, 8) = ((x - 4, 8), Right)
cubeSmall Down (8, x) | x `between` (4, 8) = ((11 - (x - 4), 8), Right)
cubeSmall Up (-1, x) | x `between` (8, 12) = ((4, 3 - (x - 8)), Down)
cubeSmall Down (12, x) | x `between` (8, 12) = ((7, 11 - x), Up)
cubeSmall Up (7, x) | x `between` (12, 16) = ((7 - (x - 12), 11), Left)
cubeSmall Down (12, x) | x `between` (12, 16) = ((7 - (x - 12), 0), Right)
cubeSmall d (y, x) = ((y, x), d)

pw :: (Pos, Dir) -> Int
pw ((y, x), d) = (y + 1) * 1000 + (x + 1) * 4 + fromEnum d

between :: Int -> (Int, Int) -> Bool
x `between` (l, r) = l <= x && x < r

enumGrid :: [[a]] -> [(a, (Int, Int))]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x c -> (c, (y, x))) [0..]

moveOrTurn :: String -> Move
moveOrTurn s = case s of
  "R" -> TurnRight
  "L" -> TurnLeft
  n -> Move (read n)

main :: IO ()
main = do
  s <- readInputFile
  let (grid, moves) = case splitOnOne "" (lines s) of
        (g, [x]) -> (g, map moveOrTurn (groupBy (\a b -> isDigit a == isDigit b) x))
        (_, []) -> error "no moves"
        (_, _:_:_) -> error "too many moves"
      height = length grid
      wall = Set.fromAscList [pos | (c, pos) <- enumGrid grid, c == '#']
      start = (0, fromJust (elemIndex '.' s))
      wrap = case height of
        200 -> dontChangeDirection wrapBig
        12 -> dontChangeDirection wrapSmall
        _ -> error ("unrecognised height " ++ show height)
      cube = case height of
        200 -> cubeBig
        12 -> cubeSmall
        _ -> error ("unrecognised height " ++ show height)
  --print (scanl' (step cube wall) (start, Right) moves)
  for_ [wrap, cube] $ \adj -> print (pw (foldl' (step adj wall) (start, Right) moves))
