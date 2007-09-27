{-# OPTIONS_GHC -fno-monomorphism-restriction #-}

module Numeric.Dimensional.Test where

import Numeric.Dimensional
import Numeric.NumType (zero, pos1, pos2, neg1, neg2)
import Prelude (($))
import qualified Prelude
import Test.HUnit

testPower = TestLabel "Power test" $ TestList
    [ TestCase $ (9 *~ one) @=? (3 *~ one) ^ pos2
    , TestCase $ (1 *~ one) @=? (12.1231 *~ one) ^ zero
    , TestCase $ (0.25 *~ one) @=? (2 *~ one) ^ neg2
    ]

testDimensionless = TestLabel "Dimensionless test" $ TestList
    [ TestCase $ (3 Prelude.** 2) *~ one @=? (3 *~ one) ** (2 *~ one)
    ]

-- Collect the test cases.
tests = TestList
    [ testPower
    , testDimensionless
    ]

main = runTestTT tests

