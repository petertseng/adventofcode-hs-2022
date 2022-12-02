import AdventOfCode (readInputFile)

score1 :: (Int, Char) -> Int
score1 (opp, meXYZ) = me + result ((me - opp) `mod` 3)
  where result 0 = 3
        result 1 = 6
        result 2 = 0
        result x = error ("Bad score1 " ++ show x ++ " from " ++ show opp ++ " and " ++ [meXYZ])
        me = case meXYZ of
          'X' -> 1
          'Y' -> 2
          'Z' -> 3
          c -> error (c : " bad score1")

score2 :: (Int, Char) -> Int
score2 (opp, meXYZ) = result + (opp - 1 + result `div` 3 - 1) `mod` 3 + 1
  where result = case meXYZ of
          'X' -> 0
          'Y' -> 3
          'Z' -> 6
          c -> error (c : " bad score2")

game :: String -> (Int, Char)
game [opp, ' ', me] = (opponent opp, me)
game g = error ("bad game " ++ g)

opponent :: Char -> Int
opponent 'A' = 1
opponent 'B' = 2
opponent 'C' = 3
opponent c = error (c : " bad opponent")

main :: IO ()
main = do
  s <- readInputFile
  let games = map game (lines s)
  print (sum (map score1 games))
  print (sum (map score2 games))
