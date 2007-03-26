Buckwalter.Dimensional.Dimensionless
Bjorn Buckwalter, bjorn.buckwalter@gmail.com
License: BSD3


= Summary =

In this module we derive instances of the Prelude's 'Num', 'Fractional'
and 'Floating' for 'Dimensionless'. This permits using literal numbers
to represent dimensionless dimensionals, and also gives us the elementary
functions for free.


= Preliminaries =

> {-# OPTIONS_GHC -fglasgow-exts #-}

> module Buckwalter.Dimensional.Dimensionless where

> import Prelude hiding (negate, (+), (-), (*), (/))
> import qualified Prelude
> import Buckwalter.Dimensional ( Dimensionless, one, negate
>                               , (+), (-), (*), (/), (*~)
>                               )


= 'Num' instance =

> instance (Num a) => Num (Dimensionless a) where
>   (+)         = (+)
>   (-)         = (-)
>   (*)         = (*)
>   abs         = fmap abs
>   signum      = fmap signum
>   negate      = negate
>   fromInteger = (*~ one) . fromInteger


= 'Fractional' instance =

> instance (Fractional a) => Fractional (Dimensionless a) where
>   (/)          = (/)
>   recip        = fmap recip
>   fromRational = (*~ one) . fromRational


= 'Floating' instance = 

We define all unary functions and let the binary functions default.

> instance (Floating a) => Floating (Dimensionless a) where
>   pi    = Prelude.pi *~ one
>   exp   = fmap exp
>   log   = fmap log
>   sqrt  = fmap sqrt
>   sin   = fmap sin
>   cos   = fmap cos
>   tan   = fmap tan
>   asin  = fmap asin
>   acos  = fmap acos
>   atan  = fmap atan
>   sinh  = fmap sinh
>   cosh  = fmap cosh
>   tanh  = fmap tanh
>   asinh = fmap asinh
>   acosh = fmap acosh
>   atanh = fmap atanh

