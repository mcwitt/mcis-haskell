module Data.Graph.Random (erdosRenyiGnp) where

import Control.Monad.Random
import Data.Graph (Bounds, Edge, Graph, buildG)

symmetric :: Bounds -> [Edge] -> Graph
symmetric bounds edges = buildG bounds [e | (u, v) <- edges, e <- [(u, v), (v, u)]]

bernoulli :: (MonadRandom m) => Double -> m Bool
bernoulli p = do
  r <- getRandomR (0.0, 1.0)
  return (r < p)

-- | Generates an Erdős–Rényi \(G_{n, p}\) random graph
erdosRenyiGnp :: (MonadRandom m) => Int -> Double -> m Graph
erdosRenyiGnp n p =
  symmetric (1, n)
    <$> filterM
      (const (bernoulli p))
      [(v1, v2) | v1 <- [1 .. n - 1], v2 <- [v1 + 1 .. n]]
