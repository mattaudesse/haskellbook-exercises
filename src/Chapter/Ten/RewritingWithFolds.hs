module Chapter.Ten.RewritingWithFolds where

import Data.List (foldl')

{-# ANN module "HLint: ignore Use or"     #-}
{-# ANN module "HLint: ignore Use concat" #-}

myOr :: [Bool] -> Bool
myOr = foldr (||) False


myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False


myElem :: Eq a => a -> [a] -> Bool
myElem e = myAny (== e)


myElem' :: Eq a => a -> [a] -> Bool
myElem' e = foldr (\e' a -> a || e == e') False

myReverse ::[a] -> [a]
myReverse = foldl' (flip (:)) []


myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\e a -> f e : a) []


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\e a -> if f e then e : a else a) []


squish :: [[a]] -> [a]
squish = foldr (++) []


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . myMap f


squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


-- NB: this is not a "total function"!
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ []       = error "Zero-length list argument!"
myMaximumBy f as@(a:_) =
    let pick acc e = case f acc e of GT -> acc
                                     EQ -> acc
                                     _  -> e
    in foldl' pick a as


-- NB: this is not a "total function"!
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ []       = error "Zero-length list argument!"
myMinimumBy f as@(a:_) =
    let pick acc e = case f acc e of GT -> e
                                     EQ -> e
                                     _  -> acc
    in foldl' pick a as
