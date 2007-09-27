module Numeric.Dimensional.Prelude 
    ( module Numeric.Dimensional
    , module Numeric.Dimensional.Quantities
    , module Numeric.Dimensional.SIUnits
    , module Numeric.NumType
    , module Prelude
    ) where

import Numeric.Dimensional hiding 
    ( Dimensional (Dimensional)
    )

import Numeric.Dimensional.Quantities

import Numeric.Dimensional.SIUnits

import Numeric.NumType 
    ( neg5, neg4, neg3, neg2, neg1, zero, pos1, pos2, pos3, pos4, pos5
    ) -- ^Used in exponents.

import Prelude hiding
    ( (+), (-), (*), (/), (^), (**)
    , abs, negate, pi, exp, log, sqrt
    , sin, cos, tan, asin, acos, atan, atan2
    , sinh, cosh, tanh, asinh, acosh, atanh
    , sum
    ) -- ^Hide definitions overridden by 'Numeric.Dimensional'.
