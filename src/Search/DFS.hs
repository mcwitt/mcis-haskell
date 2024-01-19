module Search.DFS where

import Control.Monad.ST (ST, runST)
import Data.HashTable.ST.Basic qualified as H
import Data.Hashable
import Data.Maybe (isJust)
import Data.Tree (Forest, Tree (Node))

type HashSet s k = H.HashTable s k ()

insert :: (Hashable k) => k -> HashSet s k -> ST s ()
insert k ht = H.insert ht k ()

member :: (Hashable k) => k -> HashSet s k -> ST s Bool
member k ht = isJust <$> H.lookup ht k

generate :: (a -> [a]) -> [a] -> Forest a
generate children = map go
  where
    go x = Node x $ map go (children x)

pruneBy :: (Hashable k) => (a -> k) -> Forest a -> Forest a
pruneBy mkKey xs = runST (H.new >>= chop xs)
  where
    chop [] _ = return []
    chop (Node x ts : us) m = do
      let k = mkKey x
      visited <- member k m
      if visited
        then chop us m
        else do
          insert k m
          ts' <- chop ts m
          us' <- chop us m
          return (Node x ts' : us')

prune :: (Hashable a) => Forest a -> Forest a
prune = pruneBy id

dfs :: (Hashable a) => (a -> [a]) -> [a] -> Forest a
dfs children = prune . generate children

generateM :: forall a m. (Monad m) => (a -> m [a]) -> [a] -> m (Forest a)
generateM children = mapM go
  where
    go :: a -> m (Tree a)
    go x =
      Node x <$> do
        ts <- children x
        mapM go ts

dfsM :: (Hashable a, Monad m) => (a -> m [a]) -> [a] -> m (Forest a)
dfsM children = fmap prune . generateM children
