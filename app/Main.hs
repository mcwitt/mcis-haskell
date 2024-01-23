module Main where

import Data.Graph.Random
import MCIS.McSplit
import System.Random

main :: IO ()
main =
  let randomGraph = erdosRenyiGnp 8 0.2
   in do
        setStdGen (mkStdGen 4)
        g1 <- randomGraph
        g2 <- randomGraph
        let mappings = MCIS.McSplit.mcis g1 g2
        putStrLn $ "Largest mapping size: " ++ show (length (head mappings))
        putStrLn $ "Number of largest mappings: " ++ show (length mappings)
