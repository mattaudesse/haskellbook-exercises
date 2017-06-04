module Main where

import Test.Hspec
import qualified Chapter.Six.EqInstancesSpec

main :: IO ()
main = hspec
    Chapter.Six.EqInstancesSpec.specs
