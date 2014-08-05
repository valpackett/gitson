module Main (main) where

import           Criterion.Main (bgroup, defaultMain)
import qualified GitsonBench
-- HASKELETON: import qualified New.ModuleBench

main :: IO ()
main = sequence_
    [ GitsonBench.setup
    ] >> defaultMain
    [ bgroup "Gitson" GitsonBench.benchmarks
    -- HASKELETON: , bgroup "New.Module" New.ModuleBench.benchmarks
    ] >> sequence_
    [ GitsonBench.cleanup
    ]
