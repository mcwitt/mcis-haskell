{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Data.Function (on)
import Data.List (sort)
import Data.Map qualified as Map
import MCIS.Examples (mcsplitPaperPair)
import MCIS.McSplit (mcis)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Unit tests"
    [testMcsplitExample]

testMcsplitExample :: TestTree
testMcsplitExample =
  testCase
    "MCIS on McSplit paper example"
    let (g1, g2) = mcsplitPaperPair
        expected =
          map
            Map.fromList
            [ [(2, 3), (1, 5), (4, 2), (0, 0)],
              [(0, 1), (4, 0), (3, 3), (1, 4)],
              [(2, 3), (4, 5), (0, 4), (3, 0)],
              [(2, 3), (1, 5), (4, 1), (0, 0)],
              [(0, 2), (2, 1), (3, 4), (4, 3)],
              [(1, 0), (3, 3), (4, 4), (0, 5)],
              [(0, 1), (4, 0), (1, 4), (2, 2)],
              [(1, 0), (2, 4), (0, 3), (4, 2)],
              [(2, 3), (1, 1), (0, 4), (4, 5)],
              [(1, 4), (0, 3), (2, 0), (4, 2)],
              [(2, 5), (1, 3), (4, 2), (0, 0)],
              [(2, 3), (4, 1), (3, 4), (0, 0)],
              [(0, 1), (4, 5), (1, 4), (2, 2)],
              [(4, 5), (1, 3), (2, 1), (0, 4)],
              [(0, 2), (1, 1), (3, 4), (4, 3)],
              [(0, 1), (4, 5), (1, 2), (2, 4)],
              [(3, 1), (2, 4), (0, 3), (4, 2)],
              [(1, 3), (4, 1), (3, 4), (0, 0)],
              [(0, 1), (2, 4), (4, 0), (3, 3)],
              [(0, 1), (1, 2), (4, 0), (2, 4)],
              [(4, 4), (3, 3), (2, 0), (0, 5)],
              [(1, 3), (2, 5), (4, 1), (0, 0)],
              [(3, 1), (1, 4), (0, 3), (4, 2)],
              [(1, 3), (4, 5), (0, 4), (3, 0)]
            ]
        actual = mcis g1 g2
     in (assertEqual "" `on` sort) expected actual
