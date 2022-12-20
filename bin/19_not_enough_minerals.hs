{-# LANGUAGE FlexibleContexts #-}

import AdventOfCode (readInputFile)

import Control.Arrow (first)
import Control.Monad (unless, when)
import Control.Monad.State (execState, get, modify)
import Data.Bits ((.|.), shiftL)
import Data.List (foldl')
import qualified Data.IntMap as IntMap

type Blueprint = (Int, Int, Int, Int, Int, Int)

maxGeodes :: Int -> Blueprint -> Int
maxGeodes timeLimit bp = fst $ execState (search 1 0 0 1 0 0 False False False 1 0) (0, IntMap.empty)
  where
    (oreOreCost, clayOreCost, obsidianOreCost, obsidianClayCost, geodeOreCost, geodeObsidianCost) = bp
    maxOreCost = oreOreCost `max` clayOreCost `max` obsidianOreCost `max` geodeOreCost
    search ore clay obsidian oreRobots clayRobots obsidianRobots blockOre blockClay blockObsidian time geodes = do
      let cacheKey = ore `shiftL` 40 .|. clay `shiftL` 30 .|. obsidian `shiftL` 20 .|. oreRobots `shiftL` 15 .|. clayRobots `shiftL` 10 .|. obsidianRobots `shiftL` 5 .|. time
          -- removing excess resources for the cache key is actually a slowdown
          --cacheKey = (min ore (maxOreCost + (maxOreCost - oreRobots) * timeLeftGeode)) `shiftL` 40 .|. (min clay (obsidianClayCost + (obsidianClayCost - clayRobots) * timeLeftObsidian)) `shiftL` 30 .|. obsidian `shiftL` 20 .|. oreRobots `shiftL` 15 .|. clayRobots `shiftL` 10 .|. obsidianRobots `shiftL` 5 .|. time
          timeLeftGeode = timeLimit - 2 - time
          timeLeftObsidian = max (timeLimit - 4 - time) 0

      (best, seen) <- get
      -- if out of time, or seen before, or can't beat current best, do nothing.
      unless (timeLeftGeode < 0
           || IntMap.findWithDefault (-1) cacheKey seen >= geodes
           || geodes + potential timeLimit time obsidian obsidianRobots geodeObsidianCost <= best) $ do
        -- geode bot
        let affordGeode = ore >= geodeOreCost && obsidian >= geodeObsidianCost
        when affordGeode $ do
          let geodes' = geodes + timeLimit - time - 1
          modify (first (max geodes'))
          search (ore + oreRobots - geodeOreCost) (clay + clayRobots) (obsidian + obsidianRobots - geodeObsidianCost) oreRobots clayRobots obsidianRobots False False False (time + 1) geodes'

        -- If we built a geode bot and after doing so have enough ore left over to build anything else next turn,
        -- only consider building geode bots.
        -- Otherwise do consider other bots.
        unless (affordGeode && ore + oreRobots - geodeOreCost >= maxOreCost) $ do
          -- obsidian bot
          let affordObsidian = ore >= obsidianOreCost && clay >= obsidianClayCost
          when (not blockObsidian && affordObsidian && obsidian + obsidianRobots * timeLeftGeode < geodeObsidianCost * timeLeftGeode) $
            search (ore + oreRobots - obsidianOreCost) (clay + clayRobots - obsidianClayCost) (obsidian + obsidianRobots) oreRobots clayRobots (obsidianRobots + 1) False False False (time + 1) geodes
          -- clay bot
          let affordClay = ore >= clayOreCost
          when (not blockClay && affordClay && clay + clayRobots * timeLeftObsidian < obsidianClayCost * timeLeftObsidian) $
            search (ore + oreRobots - clayOreCost) (clay + clayRobots) (obsidian + obsidianRobots) oreRobots (clayRobots + 1) obsidianRobots False False False (time + 1) geodes
          -- ore bot
          let affordOre = ore >= oreOreCost
          when (not blockObsidian && affordOre && ore + oreRobots * timeLeftGeode < maxOreCost * timeLeftGeode) $
            search (ore + oreRobots - oreOreCost) (clay + clayRobots) (obsidian + obsidianRobots) (oreRobots + 1) clayRobots obsidianRobots False False False (time + 1) geodes
          -- nothing
          -- Interestingly, the "don't do nothing if you can afford all robots" is a slowdown here.
          --when (ore < maxOreCost || clay < obsidianClayCost || obsidian < geodeObsidianCost) $
          search (ore + oreRobots) (clay + clayRobots) (obsidian + obsidianRobots) oreRobots clayRobots obsidianRobots (blockOre || affordOre) (blockClay || affordClay) (blockObsidian || affordObsidian) (time + 1) geodes

potential :: Int -> Int -> Int -> Int -> Int -> Int
potential timeLimit time obsidian obsidianRobots geodeObsidianCost = triangular (timeLeftGeode + 1) - triangular missingGeodeBots
  where timeLeftGeode = timeLimit - 2 - time
        timeLeftObsidian = timeLimit - 4 - time
        obsidian' = obsidian + timeLeftGeode * obsidianRobots + if timeLeftObsidian >= 0 then triangular (timeLeftObsidian + 1) else 0
        newGeodeRobots = min (timeLeftGeode + 1) (obsidian' `quot` geodeObsidianCost)
        missingGeodeBots = timeLeftGeode + 1 - newGeodeRobots

triangular :: Int -> Int
triangular n = n * (n + 1) `quot` 2

blueprint :: String -> Blueprint
blueprint s = case words s of
  ["Blueprint", _,
    "Each", "ore", "robot", "costs", a, "ore.",
    "Each", "clay", "robot", "costs", b, "ore.",
    "Each", "obsidian", "robot", "costs", c, "ore", "and", d, "clay.",
    "Each", "geode", "robot", "costs", e, "ore", "and", f, "obsidian."
    ] -> (read a, read b, read c, read d, read e, read f)
  _ -> error ("bad blueprint " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let bps = map blueprint (lines s)
      gs1 = map (maxGeodes 24) bps
  -- print (zip gs1 [1..])
  print (sum (zipWith (*) gs1 [1..]))

  let gs2 = map (maxGeodes 32) (take 3 bps)
  --print gs2
  print (foldl' (*) 1 gs2)
