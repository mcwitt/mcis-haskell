module McSplit (mcis, bound) where

import Control.Monad.State
import DFS (dfsM1)
import Data.Array ((!))
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Graph
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Tree

type Mapping = Map Vertex Vertex

type Label = Map Vertex Bool

type McSplit = (Mapping, Map Label [Vertex], Map Label [Vertex])

bound :: McSplit -> Int
bound (mapping, labels1, labels2) =
  let labels = Map.keysSet labels1 `Set.intersection` Map.keysSet labels2
   in length mapping
        + sum
          [ (min `on` length . (Map.! label)) labels1 labels2
            | label <- Set.toList labels
          ]

adj :: Graph -> Vertex -> Vertex -> Bool
adj g v1 v2 = v2 `elem` (g ! v1)

extend :: Graph -> Graph -> McSplit -> State Int [McSplit]
extend g1 g2 (mapping, labels1, labels2) = do
  let mappingSize = length mapping
  incumbentSize <- get
  when (length mapping > incumbentSize) (put mappingSize)
  return
    [ next
      | (l, v1s) <- Map.assocs labels1,
        v1 <- v1s,
        v2 <- Map.findWithDefault [] l labels2,
        let next =
              ( Map.insert v1 v2 mapping,
                extendLabels v1 g1 v1 labels1,
                extendLabels v1 g2 v2 labels2
              ),
        bound next >= incumbentSize
    ]
  where
    extendLabels k g v labels =
      Map.fromListWith
        (++)
        [ (Map.insert k (adj g u v) l, [u])
          | (l, vs) <- Map.assocs labels,
            u <- vs,
            u /= v
        ]

maximaBy :: (a -> a -> Ordering) -> [a] -> [a]
maximaBy cmp xs =
  let maxValue = maximumBy cmp xs
   in filter (\x -> cmp maxValue x == EQ) xs

mcis :: Graph -> Graph -> [Mapping]
mcis g1 g2 = foldTree f $ flip evalState 0 $ dfsM1 (extend g1 g2) root
  where
    f :: McSplit -> [[Mapping]] -> [Mapping]
    f (mapping, _, _) subtreeResults =
      maximaBy (compare `on` length) (mapping : concat subtreeResults)

    root =
      ( Map.empty,
        Map.singleton Map.empty (vertices g1),
        Map.singleton Map.empty (vertices g2)
      )
