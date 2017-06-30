module Main where

import Test.Hspec
import qualified Chapter.Six.EqInstancesSpec
import qualified Chapter.Ten.RewritingWithFoldsSpec

main :: IO ()
main = hspec $ do
    Chapter.Six.EqInstancesSpec.specs
    Chapter.Ten.RewritingWithFoldsSpec.specs
