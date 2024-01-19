import Criterion.Main
import McSplit (mcis)
import McSplit.Examples (mcsplitPaperPair)

main =
  defaultMain
    [ bgroup
        "mcis"
        [mcisOnMcsplitPaperPair]
    ]

mcisOnMcsplitPaperPair =
  let (g1, g2) = mcsplitPaperPair
   in bench "McSplit paper pair" $ nf (mcis g1) g2
