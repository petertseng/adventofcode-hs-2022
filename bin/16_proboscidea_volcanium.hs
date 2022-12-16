import AdventOfCode (readInputFile)
import AdventOfCode.Search (bfs)

import Control.Arrow (second)
import Data.Array.Unboxed ((!), UArray, accumArray, array, assocs, bounds, elems, listArray)
import Data.Bits ((.&.), (.|.), bit, complement, countTrailingZeros)
import Data.Either (rights)
import Data.List (dropWhileEnd, find, sortOn, tails)
import qualified Data.Map as Map
import Data.Tuple (swap)

type Tunnel = String

bestsPerOpened :: UArray Int Int -> UArray (Int, Int) Int -> Int -> UArray Int Int
bestsPerOpened rates dists time = accumArray max 0 (0, roomMask) (search aa time 0 0)
  where
    aa = snd (bounds rates) + 1
    roomMask = bit (snd (bounds rates) + 1) - 1
    search loc timeLeft open flow = (open, flow) : concatMap tryMove (setBits (roomMask .&. complement open))
      where
        tryMove destBit =
          let dest = countTrailingZeros destBit
              timeLeft' = timeLeft - dists ! (loc, dest) - 1
          in if timeLeft' > 0
            then search dest timeLeft' (open .|. destBit) (flow + rates ! dest * timeLeft')
            else []

setBits :: Int -> [Int]
setBits 0 = []
setBits n = lsb : setBits (n .&. complement lsb)
  where lsb = n .&. (-n)

-- awkward manual list work to get early termination
-- it helps, but isn't nearly as impactful as in other languages
-- (only about a 20% speedup)
-- in addition, only terminating thee outer loop seems to help.
-- terminating the inner loop (bps1) doesn't seem to at all.
bestPairSatisfying :: (a -> a -> Bool) -> (a -> Int) -> [a] -> Maybe (a, a)
bestPairSatisfying ok score = fmap snd . bps' Nothing . sortOn (\x -> -score x)
  where bps' best [] = best
        bps' best (x:_) | maybeScore best >= score x * 2 = best
        bps' best (x:xs) = let best' = bps1 xs in bps' (if maybeScore best' > maybeScore best then best' else best) xs
          where bps1 ys = fmap (\y -> (score x + score y, (x, y))) (find (ok x) ys)
          --where bps1 (y:_) | maybeScore best >= score x + score y = Nothing
                --bps1 (y:_) | ok x y = Just (score x + score y, (x, y))
                --bps1 (_:ys) = bps1 ys
                --bps1 [] = Nothing
        maybeScore = maybe 0 fst

tunnel :: Tunnel -> (Tunnel, (Int, [Tunnel]))
tunnel s = case words s of
  ("Valve":v:"has":"flow":('r':'a':'t':'e':'=':r):"tunnels":"lead":"to":"valves":vs) -> (v, (read (dropWhileEnd (== ';') r), map (dropWhileEnd (== ',')) vs))
  ["Valve", v, "has", "flow", 'r':'a':'t':'e':'=':r, "tunnel", "leads", "to", "valve", v2] -> (v, (read (dropWhileEnd (== ';') r), [v2]))
  _ -> error ("Bad tunnel " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let tunnels = map tunnel (lines s)
      nonzero = filter ((> 0) . fst . snd) tunnels
      nonzeroNames = map fst nonzero
      ids = Map.fromList (zip (nonzeroNames ++ ["AA"]) [0..])
      dests = Map.fromList (map (second snd) tunnels)
      rates = listArray (0, length nonzero - 1) (map (fst . snd) nonzero)
      distsFrom k = Map.fromList (map swap (rights (bfs (dests Map.!) (`elem` nonzeroNames) k)))
      -- dists ! (a, b) is distance from a to b.
      -- AA is present as a source but not as a destination.
      dists :: UArray (Int, Int) Int
      dists = array ((0, 0), (length nonzeroNames, length nonzeroNames - 1))
        [((i, ids Map.! dst), dist) | (src, i) <- Map.assocs ids, (dst, dist) <- Map.assocs (distsFrom src)]
  print (maximum (elems (bestsPerOpened rates dists 30)))

  let routes = bestsPerOpened rates dists 26
      earlyTerm = True
  if earlyTerm
    then case bestPairSatisfying (\(l, _) (r, _) -> l .&. r == 0) snd (assocs routes) of
      Just ((_, l), (_, r)) -> print (l + r)
      Nothing -> error "didn't find"
    else print (maximum [pres1 + pres2
      | (open1, pres1):ys <- tails (assocs routes)
      , (open2, pres2) <- ys
      , open1 .&. open2 == 0
      ])
