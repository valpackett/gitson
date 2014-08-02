module Main (main) where

import           Criterion.Main (bgroup, defaultMain)
import qualified GitsonBench
-- HASKELETON: import qualified New.ModuleBench

main :: IO ()
main = defaultMain
    [ bgroup "Gitson" GitsonBench.benchmarks
    -- HASKELETON: , bgroup "New.Module" New.ModuleBench.benchmarks
    ]
