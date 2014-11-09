module Main (main) where

import           Criterion.Main (bgroup, defaultMain)
import qualified GitsonBench

main :: IO ()
main = sequence_
    [ GitsonBench.setup
    ] >> defaultMain
    [ bgroup "Gitson" GitsonBench.benchmarks
    ] >> sequence_
    [ GitsonBench.cleanup
    ]
