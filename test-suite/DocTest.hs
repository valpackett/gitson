module Main (main) where

import           System.FilePath.Glob (glob)
import           Test.DocTest

main :: IO ()
main = glob "library/**/*.hs" >>= doctest
