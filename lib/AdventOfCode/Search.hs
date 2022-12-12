{-# LANGUAGE TupleSections #-}

module AdventOfCode.Search (
  bfs,
) where

import qualified Data.Set as Set

bfs :: Ord a => (a -> [a]) -> (a -> Bool) -> a -> [Either Int (Int, a)]
bfs neigh goal start = bfs' 0 Set.empty (Set.singleton start)
  where bfs' gen _ s | Set.null s = [Left (gen - 1)]
        bfs' gen seen front = map (Right . (gen,)) goals ++ bfs' (gen + 1) seen' front'
          where goals = filter goal (Set.toList front)
                seen' = seen `Set.union` front
                front' = Set.fromList (concatMap (filter (`Set.notMember` seen') . neigh) front)
