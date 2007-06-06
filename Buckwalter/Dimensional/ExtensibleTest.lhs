> {-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

> module Buckwalter.Dimensional.Test where

> {- 
> import Prelude hiding
>   ((*), (/), (+), (-), (^), sqrt, negate, pi, sin, cos, exp)
> --import Buckwalter.NumType (NumType, Add, Sub, Halve, Negate, Zero, Pos1, Neg1) 
> import Buckwalter.NumType ( Zero, Pos1, Neg1, pos3, neg3 )
> -}
> import Buckwalter.Dimensional.Prelude
> import Buckwalter.Dimensional.Extensible
> import Buckwalter.Dimensional ( Dimensional (Dimensional) )
> import Buckwalter.NumType ( Zero, Pos1, Neg1 )
> import qualified Prelude

= Setting up the problem domain =

For testing we will use apples, oranges and peaches.

> data TApples -- Type tag.
> type DApples  = DExt TApples Pos1 DOne
> type Apples   = Quantity DApples

> data TOranges -- Type tag.
> type DOranges = DExt TApples Zero (DExt TOranges Pos1 DOne)
> type Oranges  = Quantity DOranges

> data TPeaches -- Type tag.
> type DPeaches = DExt TApples Zero (DExt TOranges Zero (DExt TPeaches Pos1 DOne))
> type Peaches  = Quantity DPeaches

And the corresponding units.

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

> --f'' = m / a
> --f' = a * o

> {-
> f = a / o * p

> foo1 :: Quantity (DExt TApples Pos1 (DExt TOranges Neg1 (DLength))) Double
> foo1 = a / o * m

> foo2 :: Double
> foo2 = a * m / a /~ meter

> foo3 :: Length Double
> foo3 = a * m / a + m
> -- -}
