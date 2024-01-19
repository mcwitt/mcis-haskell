{-# LANGUAGE BlockArguments #-}

module DFS where

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

prune :: forall a. (Hashable a) => Forest a -> Forest a
prune xs = runST (H.new >>= chop xs)
  where
    chop [] _ = return []
    chop (Node x ts : us) m = do
      visited <- member x m
      if visited
        then chop us m
        else do
          insert x m
          ts' <- chop ts m
          us' <- chop us m
          return (Node x ts' : us')

dfs :: (Hashable a) => (a -> [a]) -> [a] -> Forest a
dfs children = prune . generate children

dfs1 :: (Hashable a) => (a -> [a]) -> a -> Tree a
dfs1 children = head . dfs children . (: [])

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

dfsM1 :: (Hashable a, Monad m) => (a -> m [a]) -> a -> m (Tree a)
dfsM1 children = fmap head . dfsM children . (: [])
