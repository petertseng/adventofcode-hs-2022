{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module AdventOfCode.Search (
  astar,
  bfs,
) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

bfs :: Ord a => (a -> [a]) -> (a -> Bool) -> a -> [Either Int (Int, a)]
bfs neigh goal start = bfs' 0 Set.empty (Set.singleton start)
  where bfs' gen _ s | Set.null s = [Left (gen - 1)]
        bfs' gen seen front = map (Right . (gen,)) goals ++ bfs' (gen + 1) seen' front'
          where goals = filter goal (Set.toList front)
                seen' = seen `Set.union` front
                front' = Set.fromList (filter (`Set.notMember` seen') (concatMap neigh front))

astar :: forall a. Ord a => (a -> [(Int, a)]) -> (a -> Int) -> (a -> Bool) -> a -> Maybe Int
astar neigh heur goal start = astar' Set.empty (pqSingleton (heur start) (0, start))
  where astar' known tentative = pqPop tentative >>= (\((dist, cur), tent) -> expand known dist cur tent)
        expand :: Set a -> Int -> a -> PQ (Int, a) -> Maybe Int
        expand _ dist current _ | goal current = Just dist
        expand known _ current tentative | current `Set.member` known = astar' known tentative
        expand known dist current tentative = astar' (Set.insert current known) tentative'
          where tentative' = pqUnion tentative (map estimateDist (filter unknown (neigh current)))
                unknown (_, n) = n `Set.notMember` known
                estimateDist (stepDist, n) = (dist + stepDist + heur n, (dist + stepDist, n))

type PQ a = (Int, [a], IntMap [a])

pqSingleton :: Int -> a -> PQ a
pqSingleton pri x = (pri, [x], IntMap.empty)

pqInsert :: Int -> a -> PQ a -> PQ a
pqInsert pri _ (curPri, _, _) | pri < curPri = error ("non-monotonic add " ++ show pri ++ " vs " ++ show curPri)
pqInsert pri x (curPri, xs, qs) | pri == curPri = (curPri, x:xs, qs)
pqInsert pri x (curPri, xs, qs) = (curPri, xs, IntMap.alter (maybe (Just [x]) (Just . (x:))) pri qs)

pqUnion :: PQ a -> [(Int, a)] -> PQ a
pqUnion = foldl' (\pq' (pri, x) -> pqInsert pri x pq')

pqPop :: PQ a -> Maybe (a, PQ a)
pqPop (pri, x:xs, qs) = Just (x, (pri, xs, qs))
pqPop (_, [], qs) = case IntMap.minViewWithKey qs of
  Nothing -> Nothing
  Just ((pri', []), _) -> error ("bad queue " ++ show pri')
  Just ((pri', x:xs), qs') -> Just (x, (pri', xs, qs'))
