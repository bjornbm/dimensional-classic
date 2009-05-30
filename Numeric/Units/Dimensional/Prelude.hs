module Numeric.Units.Dimensional.Prelude
    ( module Numeric.Units.Dimensional
    , module Numeric.Units.Dimensional.Quantities
    , module Numeric.Units.Dimensional.SIUnits
    , module Numeric.NumType
    , module Prelude
    ) where

import Numeric.Units.Dimensional hiding
    ( Dimensional (Dimensional)
    )

import Numeric.Units.Dimensional.Quantities

import Numeric.Units.Dimensional.SIUnits

import Numeric.NumType
    ( neg5, neg4, neg3, neg2, neg1, zero, pos1, pos2, pos3, pos4, pos5
    )  -- Used in exponents.

import Prelude hiding
    ( (+), (-), (*), (/), (^), (**)
    , abs, negate, pi, exp, log, sqrt
    , sin, cos, tan, asin, acos, atan, atan2
    , sinh, cosh, tanh, asinh, acosh, atanh
    , sum
    )  -- Hide definitions overridden by 'Numeric.Dimensional'.
