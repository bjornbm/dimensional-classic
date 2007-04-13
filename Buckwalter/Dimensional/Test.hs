{-# OPTIONS_GHC -fno-monomorphism-restriction #-}

module Buckwalter.Dimensional.Test where

import Buckwalter.Dimensional
import Buckwalter.Dimensional.Dimensionless
import Buckwalter.NumType (zero, pos1, pos2, neg1, neg2)
import Prelude (($), (**))
import Test.HUnit

testPower = TestLabel "Power test" $ TestList
    [ TestCase $ (9 *~ one) @=? (3 *~ one) ^ pos2
    , TestCase $ (1 *~ one) @=? (12.1231 *~ one) ^ zero
    , TestCase $ (0.25 *~ one) @=? (2 *~ one) ^ neg2
    ]

testDimensionless = TestLabel "Dimensionless test" $ TestList
    [ TestCase $ (3 ** 2) @=? (3 *~ one) ** (2 *~ one)
    , TestCase $ (3 ** 2) @=? (3 *~ one) ** 2
    , TestCase $ (3 ** 2) @=? 3 ** (2 *~ one)
    ]

-- Collect the test cases.
tests = TestList
    [ testPower
    , testDimensionless
    ]

main = runTestTT tests

