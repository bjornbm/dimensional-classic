> {-# LANGUAGE EmptyDataDecls, FlexibleInstances #-}

> module Numeric.Units.Dimensional.ExtensibleTest where

> import Numeric.Units.Dimensional.Prelude
> import Numeric.Units.Dimensional.Extensible
> import Numeric.Units.Dimensional ( Dimensional (Dimensional), dimUnit )
> import Numeric.NumType ( NumType, Zero, Pos1, Neg1 )
> import Test.HUnit
> import qualified Prelude


= Setting up the problem domain =

For testing we will use apples, oranges and peaches. We define the
type tags and show instances for each.

> data TApples  -- Type tag.
> type DApples  = DExt TApples Pos1 DOne
> type Apples   = Quantity DApples

> data TOranges -- Type tag.
> type DOranges = DExt TApples Zero (DExt TOranges Pos1 DOne)
> type Oranges  = Quantity DOranges

> data TPeaches -- Type tag.
> type DPeaches = DExt TApples Zero (DExt TOranges Zero (DExt TPeaches Pos1 DOne))
> type Peaches  = Quantity DPeaches

Define show instances.

> instance (NumType n, Show d) => Show (DExt TApples n d) where
>   show = showDExt "apple"

> instance (NumType n, Show d) => Show (DExt TOranges n d) where
>   show = showDExt "orange"

> instance (NumType n, Show d) => Show (DExt TPeaches n d) where
>   show = showDExt "peaches"

Finally the base units.

> apple  :: Num a => Unit DApples a
> apple  = Dimensional 1
> orange :: Num a => Unit DOranges a
> orange = Dimensional 1
> peach  :: Num a => Unit DPeaches a
> peach  = Dimensional 1


= Test values =

> a = 1 *~ apple
> o = 2 *~ orange
> m = 3 *~ meter
> p = 4 *~ peach


= Stuff we expect to compile =

> f = a / o * p
> f' = a * o
> f'' = m / a

> foo1 :: Quantity (DExt TApples Pos1 (DExt TOranges Neg1 (DLength))) Double
> foo1 = a / o * m

> foo2 :: Double
> foo2 = a * m / a /~ meter

> foo3 :: Length Double
> foo3 = a * m / a + m


Finally a HUnit test case.

> testShow = TestLabel "Test 'Show' instance" $ TestList
>   [ TestCase $ show (1 *~ apple) @?= "1 apple"
>   , TestCase $ show (2 *~ orange) @?= "2 orange"
>   , TestCase $ show (2.0 *~ (apple / second)) @?= "2.0 apple s^-1"
>   , TestCase $ show (2.0 *~ (meter ^ pos2 / peach ^ pos2)) @?= "2.0 peaches^-2 m^2"
>   , TestCase $ show (2.0 *~ (apple ^ pos2 / peach ^ pos2)) @?= "2.0 apple^2 peaches^-2"
>   , TestCase $ show (undefined :: DApples) @?= "apple"
>   ]

Main function.

> main = do
>   putStrLn "If I compiled I'm mostly OK!"
>   runTestTT $ TestList [testShow]

