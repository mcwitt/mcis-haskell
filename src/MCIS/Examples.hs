module MCIS.Examples where

import Data.Graph (Bounds, Edge, Graph, buildG)

symmetric :: Bounds -> [Edge] -> Graph
symmetric bounds edges = buildG bounds [e | (u, v) <- edges, e <- [(u, v), (v, u)]]

mcsplitPaperPair =
  let g1 =
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
   in (g1, g2)
