module McSplit (mcis, bound) where

import Control.Monad.State
import DFS (generateM, pruneBy)
import Data.Array ((!))
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Graph (Graph, Vertex, vertices)
import Data.List (delete, partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Tree

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
                  ( \(vs1, vs2) ->
                      let (vs1a, vs1b) = partition (adj g1 v1) (delete v1 vs1)
                          (vs2a, vs2b) = partition (adj g2 v2) (delete v2 vs2)
                       in [(vs1a, vs2a), (vs1b, vs2b)]
                  )
                  labelClasses
              ),
        bound next >= incumbentSize
    ]

maximaBy :: (a -> a -> Ordering) -> [a] -> [a]
maximaBy cmp xs =
  let maxValue = maximumBy cmp xs
   in filter (\x -> cmp maxValue x == EQ) xs

mcis :: Graph -> Graph -> [Mapping]
mcis g1 g2 = foldTree f $ flip evalState 0 $ dfs (extend g1 g2) root
  where
    dfs children v = head . pruneBy fst <$> generateM children [v]

    f :: McSplit -> [[Mapping]] -> [Mapping]
    f (mapping, _) ts = maximaBy (compare `on` length) (mapping : concat ts)

    root = (Map.empty, [(vertices g1, vertices g2)])
