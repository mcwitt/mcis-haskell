{-# LANGUAGE BlockArguments #-}

import Criterion.Main
import Data.Graph.Random (erdosRenyiGnp)
import MCIS.Examples (mcsplitPaperPair)
import MCIS.McSplit qualified
import System.Random

main :: IO ()
main =
  defaultMain
    [ bgroup
        "McSplit paper example"
        ( let (g1, g2) = mcsplitPaperPair
           in [bench "McSplit" $ nf (MCIS.McSplit.mcis g1) g2]
        ),
      bgroup
        "Random graphs"
        ( let withRandomPair n p =
                env do
                  setStdGen (mkStdGen 123)
                  let rg = erdosRenyiGnp n p
                  g1 <- rg
                  g2 <- rg
                  return (g1, g2)
              randomPairBenchmark n p = withRandomPair n p $ \ ~(g1, g2) ->
                bench ("n=" ++ show n ++ ", " ++ "p=" ++ show p) $
                  nf (MCIS.McSplit.mcis g1) g2
           in [randomPairBenchmark 10 0.1]
        )
    ]
