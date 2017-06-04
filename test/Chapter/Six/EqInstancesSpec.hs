module Chapter.Six.EqInstancesSpec (specs) where

import Test.Hspec
import Test.QuickCheck
import Chapter.Six.EqInstances


tisAnInteger :: SpecWith ()
tisAnInteger = describe "TisAnInteger has an `Eq` instance that" $ do
    it "works for equal integers" $
        property $ \x -> TisAn x == TisAn x

    it "works for unequal integers" $
        property $ \x y -> x == y || TisAn x /= TisAn y


twoIntegers :: SpecWith ()
twoIntegers = describe "TwoIntegers has an `Eq` instance that" $ do
    it "works for equal integers" $
        property $ \x1 x2 -> Two x1 x2 == Two x1 x2

    it "works for unequal integers" $
        property $ \x1 y1 x2 y2 -> (x1 == x2 && y1 == y2) || Two x1 y1 /= Two x2 y2


stringOrInt :: SpecWith ()
stringOrInt = describe "StringOrInt has an `Eq` instance that" $ do
    it "works for equal `TisAnInt` constructors" $
        property $ \x -> TisAnInt x == TisAnInt x

    it "works for equal `TisAString` constructors" $
        property $ \x -> TisAString x == TisAString x

    it "works for unequal `TisAnInt` constructors" $
        property $ \x y -> x == y || TisAnInt x /= TisAnInt y

    it "works for unequal `TisAString` constructors" $
        property $ \x y -> x == y || TisAString x /= TisAString y

    it "knows `TisAnInt x` may never equal `TisAString x`" $
        property $ \x y -> TisAnInt x /= TisAString y


pair :: SpecWith ()
pair = describe "Pair has an `Eq` instance that" $ do
    it "works for equal inputs" $
        property $ \x y ->
            let p =  Pair (x :: String) (y :: String)
            in  p == p

    it "works for unequal inputs" $
        property $ \x1 y1 x2 y2 ->
            let p1 =  Pair (x1 :: String) (y1 :: String)
                p2 =  Pair (x2 :: String) (y2 :: String)
            in  (x1 == x2 && y1 == y2) || p1 /= p2


tuple :: SpecWith ()
tuple = describe "Tuple has an `Eq` instance that" $ do
    it "works for equal inputs" $
        property $ \x y ->
            let t =  Tuple (x :: Integer) (y :: String)
            in  t == t

    it "works for unequal inputs" $
        property $ \x1 y1 x2 y2 ->
            let t1 =  Tuple (x1 :: Float) (y1 :: Char)
                t2 =  Tuple (x2 :: Float) (y2 :: Char)
            in  (x1 == x2 && y1 == y2) || t1 /= t2


which :: SpecWith ()
which = describe "Which has an `Eq` that" $ do
    it "works for equal `ThisOne` constructors" $
        property $ \x -> ThisOne x == ThisOne (x :: Int)

    it "works for equal `ThatOne` constructors" $
        property $ \x -> ThatOne x == ThatOne (x :: Int)

    it "works for unequal `ThisOne` constructors" $
        property $ \x y -> x == y || ThisOne x /= ThisOne (y :: Int)

    it "works for unequal `ThatOne` constructors" $
        property $ \x y -> x == y || ThatOne x /= ThatOne (y :: Int)

    it "knows `ThisOne x` may never equal `ThatOne x`" $
        property $ \x -> ThisOne x /= ThatOne (x :: Int)


eitherOr :: SpecWith ()
eitherOr = describe "EitherOr has an `Eq` instance that" $ do
    it "works for equal `Hello` constructors" $
        property $ \x -> Hello x == (Hello x :: EitherOr Int String)

    it "works for equal `Goodbye` constructors" $
        property $ \x -> Goodbye x == (Goodbye x :: EitherOr Int String)

    it "works for unequal `Hello` constructors" $
        property $ \x y -> x == y || Hello x /= (Hello y :: EitherOr Int String)

    it "works for unequal `Goodbye` constructors" $
        property $ \x y -> x == y || Goodbye x /= (Goodbye y :: EitherOr Int String)

    it "knows that `Hello x` may never equal `Goodbye x`" $
        property $ \x y -> Hello x /= (Goodbye y :: EitherOr Int String)


specs :: SpecWith ()
specs = describe "Chapter.Six.EqInstances" $ do
    tisAnInteger
    twoIntegers
    stringOrInt
    pair
    tuple
    which
    eitherOr
