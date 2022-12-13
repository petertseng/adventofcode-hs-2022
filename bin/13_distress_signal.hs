import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Control.Arrow (first)
import Data.Char (isDigit)

data Packet = List [Packet] | Val Int deriving (Eq, Show)
data ListParseState = Open | Elt | Comma deriving (Show)

instance Ord Packet where
  compare (Val l) (Val r) = compare l r
  compare (List l) (List r) = compare l r
  compare l@(List _) r@(Val _) = compare l (List [r])
  compare l@(Val _) r@(List _) = compare (List [l]) r

mustPacket :: String -> Packet
mustPacket s = case packet s of
  (List l, "") -> List l
  (Val n, _) -> error ("number " ++ show n)
  (_, s') -> error ("unparsed " ++ s')

packet :: String -> (Packet, String)
packet ('[':s) = case listElements s of
  (l, ']':s') -> (List l, s')
  (_, s') -> error ("unclosed list " ++ s')
packet s@(d:_) | isDigit d = let (n, s') = span isDigit s in (Val (read n), s')
packet s = error ("bad packet " ++ s)

listElements :: String -> ([Packet], String)
listElements = elts Open
  where elts _ s@(x:_) | isDigit x || x == '[' = let (v, s') = packet s in first (v :) (elts Elt s')
        elts Elt (',':s) = elts Comma s
        elts prev (',':_) = error ("bad comma after " ++ show prev)
        elts Comma (']':_) = error "bad closing bracket after comma"
        elts Comma [] = error "string ended after comma"
        elts _ s@(']':_) = ([], s)
        elts _ "" = ([], "")
        elts _ s = error ("bad list element " ++ s)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let pairs = map (map mustPacket) (splitOn "" (lines s))
      sum2 [a, b] i = if a < b then i else 0 :: Int
      sum2 x _ = error (show x ++ " didn't have 2 packets")
  print (sum (zipWith sum2 pairs [1..]))
  let packets = concat pairs
      mark1 = count (< List [List [Val 2]]) packets + 1
      mark2 = count (< List [List [Val 6]]) packets + 2
  print (mark1 * mark2)
