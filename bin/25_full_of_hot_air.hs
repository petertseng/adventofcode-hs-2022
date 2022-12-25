import AdventOfCode (readInputFile)

import Data.List (foldl', mapAccumL)

fromSnafu :: String -> Int
fromSnafu = foldl' (\a c -> a * 5 + snaf c) 0
  where snaf '2' = 2
        snaf '1' = 1
        snaf '0' = 0
        snaf '-' = -1
        snaf '=' = -2
        snaf c = error (c : " is not SNAFU")

toSnafu :: Int -> String
toSnafu = reverse . snd . mapAccumL snaf False . digitsInBase 5
  where snaf carry d = let d' = d + if carry then 1 else 0 in (d' >= 3, "012=-0" !! d')

digitsInBase :: Int -> Int -> [Int]
digitsInBase _ 0 = []
digitsInBase b n = let (n', d) = n `divMod` b in d : digitsInBase b n'

main :: IO ()
main = do
  s <- readInputFile
  let snafs = sum (map fromSnafu (lines s))
  putStrLn (toSnafu snafs)
