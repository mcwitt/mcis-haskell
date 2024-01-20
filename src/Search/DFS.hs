module Search.DFS where

import Control.Monad.State
import Data.HashSet qualified as HS
import Data.Hashable
import Data.Tree (Forest, Tree (Node))

generate :: (a -> [a]) -> [a] -> Forest a
generate children = map go
  where
    go x = Node x $ map go (children x)

pruneBy :: (Hashable k) => (a -> k) -> Forest a -> Forest a
pruneBy mkKey xs = evalState (chop xs) HS.empty
  where
    chop [] = return []
    chop (Node x ts : us) = do
      let k = mkKey x
      visited <- gets (HS.member k)
      if visited
        then chop us
        else do
          modify (HS.insert k)
          ts' <- chop ts
          us' <- chop us
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
