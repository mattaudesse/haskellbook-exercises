module Chapter.Ten.RewritingWithFoldsSpec (specs) where

import Test.Hspec
import Chapter.Ten.RewritingWithFolds


myOrSpecs :: SpecWith ()
myOrSpecs = describe "`myOr` returns" $ do
    it "`True` if any `Bool` in its argument is `True`" $ do
        myOr [True, True, True]   `shouldBe` True
        myOr [True]               `shouldBe` True
        myOr [False, False, True] `shouldBe` True

    it "`False` if all `Bool`s in its argument are `False`" $
        myOr [False, False] `shouldBe` False

    it "`False` if its argument is `[]`" $
        myOr [] `shouldBe` False


myAnySpecs :: SpecWith ()
myAnySpecs = describe "`myAny` returns" $ do
    it "`True` if `a -> Bool` applied to any values in list returns `True`" $ do
        myAny even [1, 1, 3, 4, 7, 13 :: Int] `shouldBe` True
        myAny even [2, 4, 6, 4, 7, 13 :: Int] `shouldBe` True

    it "`False` if `a -> Bool` applied to all values in list returns `False`" $ do
        myAny even [1, 1, 3, 7, 13 :: Int] `shouldBe` False
        myAny even [1 :: Int]              `shouldBe` False

    it "`False` if `a -> Bool` is applied to an empty list" $
        myAny even ([] :: [Int]) `shouldBe` False


myElemSpecs :: SpecWith ()
myElemSpecs = describe "`myElem` reimplements `elem` with `myAny` and" $ do
    it "returns `True` if arg one is an element of the list in arg two" $ do
        myElem 1   [1..10 :: Int]  `shouldBe` True
        myElem 'a' "Cats and dogs" `shouldBe` True

    it "returns `False` if arg one isn't an element of the list in arg two" $ do
        myElem 0   [1..10 :: Int]  `shouldBe` False
        myElem 'x' "Cats and dogs" `shouldBe` False


myElem'Specs :: SpecWith ()
myElem'Specs = describe "`myElem'` reimplements `elem` with `foldr` and" $ do
    it "returns `True` if arg one is an element of the list in arg two" $ do
        myElem' 1   [1..10 :: Int]  `shouldBe` True
        myElem' 'a' "Cats and dogs" `shouldBe` True

    it "returns `False` if arg one isn't an element of the list in arg two" $ do
        myElem' 0   [1..10 :: Int]  `shouldBe` False
        myElem' 'x' "Cats and dogs" `shouldBe` False


myReverseSpecs :: SpecWith ()
myReverseSpecs = describe "`myReverse` reimplements `reverse` and" $
    it "reverses its argument" $ do
        myReverse "blah"        `shouldBe` "halb"
        myReverse [1..5 :: Int] `shouldBe` [5,4,3,2,1]


myMapSpecs :: SpecWith ()
myMapSpecs = describe "`myMap` reimplements `map` in terms of `foldr` and" $
    it "behaves as you'd expect `map` to" $ do
        myMap (\x -> x * x) [1..5 :: Int] `shouldBe` [1,4,9,16,25]
        myMap (\x -> x * x) ([] :: [Int]) `shouldBe` []


myFilterSpecs :: SpecWith ()
myFilterSpecs = describe "`myFilter` reimplements `filter` in terms of foldr` and" $
    it "behaves as you'd expect `filter` to" $ do
        myFilter even [1..10 :: Int] `shouldBe` [2,4,6,8,10]
        myFilter even ([] :: [Int])  `shouldBe` []


squishSpecs :: SpecWith ()
squishSpecs = describe "`squish` is a combinator that" $
    it "flattens a list of lists into a list" $ do
        squish [[1,2], [3,4], [5,6 :: Int]] `shouldBe` [1,2,3,4,5,6]
        squish [[] :: [Int]]                `shouldBe` []
        squish ([] :: [[Int]])              `shouldBe` []


squishMapSpecs :: SpecWith ()
squishMapSpecs = describe "`squishMap` is a combinator that" $
    it "maps a function over a list and concatenates the results" $ do
        squishMap (\x -> [1, x, 3]) [2 :: Int] `shouldBe` [1,2,3]

        squishMap (\x -> "WO " ++ [x] ++ " OT ") "blah" `shouldBe`
            "WO b OT WO l OT WO a OT WO h OT "


squishAgainSpecs :: SpecWith ()
squishAgainSpecs = describe "`squishAgain` is `squish` in terms of `squishMap` that" $
    it "flattens a list of lists into a list" $ do
        squishAgain [[1,2], [3,4], [5,6 :: Int]] `shouldBe` [1,2,3,4,5,6]
        squishAgain [[] :: [Int]]                `shouldBe` []
        squishAgain ([] :: [[Int]])              `shouldBe` []


myMaximumBySpecs :: SpecWith ()
myMaximumBySpecs = describe "`myMaximumBy` is a combinator that" $
    it "takes a comparison func and list and returns the last value by `GT`" $ do
        myMaximumBy (\_ _ -> GT) [1..10 :: Int] `shouldBe` 1
        myMaximumBy (\_ _ -> LT) [1..10 :: Int] `shouldBe` 10
        myMaximumBy compare      [1..10 :: Int] `shouldBe` 10


myMinimumBySpecs :: SpecWith ()
myMinimumBySpecs = describe "`myMinimumBy` is a combinator that" $
    it "takes a comparison func and list and returns the last value by `LT`" $ do
        myMinimumBy (\_ _ -> GT) [1..10 :: Int] `shouldBe` 10
        myMinimumBy (\_ _ -> LT) [1..10 :: Int] `shouldBe` 1
        myMinimumBy compare      [1..10 :: Int] `shouldBe` 1


specs :: SpecWith ()
specs = describe "Chapter.Ten.RewritingWithFolds" $ do
    myOrSpecs
    myAnySpecs
    myElemSpecs
    myElem'Specs
    myReverseSpecs
    myMapSpecs
    myFilterSpecs
    squishSpecs
    squishMapSpecs
    squishAgainSpecs
    myMaximumBySpecs
    myMinimumBySpecs
