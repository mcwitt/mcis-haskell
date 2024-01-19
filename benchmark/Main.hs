import Criterion.Main
import MCIS.Examples (mcsplitPaperPair)
import MCIS.McSplit (mcis)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "mcis"
        [bgroup "mcsplit" [mcisOnMcsplitPaperPair]]
    ]

mcisOnMcsplitPaperPair :: Benchmark
mcisOnMcsplitPaperPair =
  let (g1, g2) = mcsplitPaperPair
   in bench "McSplit paper pair" $ nf (mcis g1) g2
