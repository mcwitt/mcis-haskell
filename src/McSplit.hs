{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module McSplit () where

import Control.Monad.ST (ST, runST)
import Data.Array ((!))
import Data.Graph (Bounds, Edge, Graph, Vertex, buildG, vertices)
import Data.HashTable.ST.Basic qualified as H
import Data.Hashable
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Tree (Forest, Tree (Node), levels)

type Mapping = Map Vertex Vertex

type Label = Map Vertex Bool

type McSplit = (Mapping, Map Label [Vertex], Map Label [Vertex])

bound :: Map Label [Vertex] -> Map Label [Vertex] -> Int
bound labels1 labels2 =
  let labels = Map.keysSet labels1 `Set.union` Map.keysSet labels2
   in sum
        [ min
            (length (Map.findWithDefault [] label labels1))
            (length (Map.findWithDefault [] label labels2))
          | label <- Set.toList labels
        ]

empty :: Graph -> Graph -> McSplit
empty g1 g2 =
  ( Map.empty,
    Map.singleton Map.empty (vertices g1),
    Map.singleton Map.empty (vertices g2)
  )

adj :: Graph -> Vertex -> Vertex -> Bool
adj g v1 v2 = v2 `elem` (g ! v1)

extend :: Graph -> Graph -> McSplit -> [McSplit]
extend g1 g2 node@(mapping, labels1, labels2)
  | bound labels1 labels2 == 0 = [node]
  | otherwise =
      [ ( Map.insert v1 v2 mapping,
          extendLabels v1 g1 v1 labels1,
          extendLabels v1 g2 v2 labels2
        )
        | (l, v1s) <- Map.assocs labels1,
          v1 <- v1s,
          v2 <- Map.findWithDefault [] l labels2
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

symmetric :: Bounds -> [Edge] -> Graph
symmetric bounds edges = buildG bounds [e | (u, v) <- edges, e <- [(u, v), (v, u)]]

step g1 g2 = nub . concatMap (extend g1 g2)

test1 =
  filter
    (\(mapping, _, _) -> traverse (`Map.lookup` mapping) [0, 1, 2] == Just [0, 3, 5])
    $ (!! 3)
    $ iterate f
    $ return (empty g1 g2)
  where
    g1, g2 :: Graph
    g1 =
      symmetric
        (0, 4)
        [ (0, 3),
          (0, 4),
          (1, 2),
          (1, 4),
          (2, 4)
        ]
    g2 =
      symmetric
        (0, 5)
        [ (0, 1),
          (0, 2),
          (0, 4),
          (1, 3),
          (1, 5),
          (2, 3),
          (2, 4),
          (2, 5),
          (3, 5),
          (4, 5)
        ]
    f = step g1 g2

type HashSet s k = H.HashTable s k ()

insert :: (Hashable k) => k -> HashSet s k -> ST s ()
insert k ht = H.insert ht k ()

member :: (Hashable k) => k -> HashSet s k -> ST s Bool
member k ht = isJust <$> H.lookup ht k

generate :: (a -> [a]) -> [a] -> Forest a
generate children = map go
  where
    go x = Node x $ map go (children x)

prune :: forall a. (Hashable a) => Forest a -> Forest a
prune ts = runST do
  m <- H.new
  chop m ts
  where
    chop _ [] = return []
    chop m (Node x ts : us) = do
      visited <- member x m
      if visited
        then chop m us
        else do
          insert x m
          ts' <- chop m ts
          us' <- chop m us
          return (Node x ts' : us')

dfs :: (Hashable a) => (a -> [a]) -> [a] -> Forest a
dfs children = prune . generate children

dfs1 children = head . dfs children . (: [])

g1, g2 :: Graph
g1 =
  symmetric
    (0, 4)
    [ (0, 3),
      (0, 4),
      (1, 2),
      (1, 4),
      (2, 4)
    ]
g2 =
  symmetric
    (0, 5)
    [ (0, 1),
      (0, 2),
      (0, 4),
      (1, 3),
      (1, 5),
      (2, 3),
      (2, 4),
      (2, 5),
      (3, 5),
      (4, 5)
    ]

test2 = last $ levels $ (\(mapping, _, _) -> mapping) <$> dfs1 (extend g1 g2) (empty g1 g2)

test3 = last $ levels $ (\(mapping, _, _) -> mapping) <$> dfs1 (extend g1 g2) (empty g1 g2)
  where
    g1 = symmetric (0, 4) [(0, 1), (1, 2), (2, 3), (3, 4), (4, 0)]
    g2 = symmetric (0, 4) [(0, 1), (1, 2), (2, 3), (3, 4), (4, 0)]
