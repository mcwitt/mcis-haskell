module MCIS.McSplit (mcis) where

import Control.Monad.State (State, evalState, state)
import Data.Array ((!))
import Data.Function (on)
import Data.Graph (Graph, Vertex, vertices)
import Data.List (delete, foldl', partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Tree (Tree (Node), foldTree)

type Mapping = Map Vertex Vertex

data McSplit = McSplit Mapping [([Vertex], [Vertex])]

bound :: McSplit -> Int
bound (McSplit m cs) =
  length m + sum [(min `on` length) vs1 vs2 | (vs1, vs2) <- cs]

adj :: Graph -> Vertex -> Vertex -> Bool
adj g v1 v2 = v2 `elem` (g ! v1)

extend :: Graph -> Graph -> McSplit -> State Int [McSplit]
extend g1 g2 = go
  where
    go (McSplit _ []) = return []
    go (McSplit m (([], _) : cs)) = go (McSplit m cs)
    go (McSplit m ((_, []) : cs)) = go (McSplit m cs)
    go (McSplit m ((v1 : v1s, v2s) : cs)) =
      state
        ( \n ->
            ( McSplit m ((v1s, v2s) : cs)
                : [ next
                    | v2 <- v2s,
                      let next =
                            McSplit
                              (Map.insert v1 v2 m)
                              ( concatMap
                                  ( \(u1s, u2s) ->
                                      let (u1sa, u1sb) = partition (adj g1 v1) u1s
                                          (u2sa, u2sb) = partition (adj g2 v2) u2s
                                       in [(u1sa, u2sa), (u1sb, u2sb)]
                                  )
                                  ((v1s, delete v2 v2s) : cs)
                              ),
                      bound next >= n
                  ],
              max (length m) n
            )
        )

generateM :: (Monad m) => (a -> m [a]) -> a -> m (Tree a)
generateM children node = do
  nodes <- children node
  subtrees <- mapM (generateM children) nodes
  return (Node node subtrees)

mcisSearchTree :: Graph -> Graph -> Tree McSplit
mcisSearchTree g1 g2 = evalState search 0
  where
    search = generateM (extend g1 g2) root
    root = McSplit Map.empty [(vertices g1, vertices g2)]

largestBy :: (a -> a -> Ordering) -> [a] -> [a]
largestBy _ [] = []
largestBy cmp (x : xs) = foldl' f [x] xs
  where
    f zs y = case y `cmp` head zs of
      LT -> zs
      EQ -> y : zs
      GT -> [y]

mcis :: Graph -> Graph -> [Mapping]
mcis g1 g2 = foldTree f $ mcisSearchTree g1 g2
  where
    f (McSplit m _) mss = largestBy (compare `on` length) $ m : concat mss
