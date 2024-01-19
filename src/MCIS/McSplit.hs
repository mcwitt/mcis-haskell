module MCIS.McSplit (mcis) where

import Control.Monad.State
import Data.Array ((!))
import Data.Function (on)
import Data.Graph (Graph, Vertex, vertices)
import Data.List (delete, partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Tree
import Search.DFS (generateM, pruneBy)

type Mapping = Map Vertex Vertex

type McSplit = (Mapping, [([Vertex], [Vertex])])

bound :: McSplit -> Int
bound (mapping, labelClasses) =
  length mapping + sum [(min `on` length) vs1 vs2 | (vs1, vs2) <- labelClasses]

adj :: Graph -> Vertex -> Vertex -> Bool
adj g v1 v2 = v2 `elem` (g ! v1)

extend :: Graph -> Graph -> McSplit -> State Int [McSplit]
extend g1 g2 (mapping, labelClasses) = do
  let mappingSize = length mapping
  incumbentSize <- get
  when (mappingSize > incumbentSize) (put mappingSize)
  return
    [ next
      | (v1s, v2s) <- labelClasses,
        v1 <- v1s,
        v2 <- v2s,
        let next =
              ( Map.insert v1 v2 mapping,
                concatMap
                  ( \(u1s, u2s) ->
                      let (u1sa, u1sb) = partition (adj g1 v1) (delete v1 u1s)
                          (u2sa, u2sb) = partition (adj g2 v2) (delete v2 u2s)
                       in [(u1sa, u2sa), (u1sb, u2sb)]
                  )
                  labelClasses
              ),
        bound next >= incumbentSize
    ]

mcisSearchTree :: Graph -> Graph -> Tree McSplit
mcisSearchTree g1 g2 = flip evalState 0 $ dfs (extend g1 g2) root
  where
    dfs children v = head . pruneBy fst <$> generateM children [v]
    root = (Map.empty, [(vertices g1, vertices g2)])

mcis :: Graph -> Graph -> [Mapping]
mcis g1 g2 = last $ levels $ fst <$> mcisSearchTree g1 g2
