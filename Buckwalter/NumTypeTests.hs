{-# OPTIONS_GHC -fno-monomorphism-restriction #-}

module NumTypeTests where

import Buckwalter.NumType
import Prelude hiding ((*), (/), (+), (-), negate) -- (undefined, Integral)
import qualified Prelude as P ((*), (/), (+), (-), negate)
import Test.HUnit


-- Compares a type level unary function with a value level unary function 
-- by converting 'NumType' to 'Integral'. This assumes that the 'asIntegral'
-- function is solid.
unaryTest :: (NumType n, NumType n', Num a) => (n -> n') -> (a -> a) -> n -> Test
unaryTest f f' x = TestCase $ assertEqual "Unary function Integral equivalence" 
                                          (f' ((fromIntegral . asIntegral) x)) 
                                          ((fromIntegral . asIntegral) (f x))

-- Test that conversion to 'Integral' works as expected.
testAsIntegral = TestLabel "Integral equivalence tests" $ TestList 
    [ TestCase $ -2 @=? asIntegral neg2
    , TestCase $ -1 @=? asIntegral neg1
    , TestCase $  0 @=? asIntegral zero
    , TestCase $  1 @=? asIntegral pos1
    , TestCase $  2 @=? asIntegral pos2
    ] -- By induction all other NumTypes should be good if these are.

-- Test increment and decrement for a bunch of 'NumTypes'
testIncrDecr = TestLabel "Increment and decrement tests" $ TestList
    [ t neg2
    , t neg1
    , t zero
    , t pos1
    , t pos1
    ] 
    where 
        t x = TestList
            [ unaryTest incr (P.+ 1) x
            , unaryTest decr (P.- 1) x
            ]

-- Test negation.
testNegate = TestLabel "Negation tests" $ TestList
    [ unaryTest negate P.negate neg2
    , unaryTest negate P.negate neg1
    , unaryTest negate P.negate zero
    , unaryTest negate P.negate pos1
    , unaryTest negate P.negate pos1
    ]

-- Test Halving
testHalve = TestLabel "Halving tests" $ TestList
    [ unaryTest halve (P./ 2) neg4
    , unaryTest halve (P./ 2) neg2
    , unaryTest halve (P./ 2) zero
    , unaryTest halve (P./ 2) neg2
    , unaryTest halve (P./ 2) neg4
    ]

-- Collect the test cases.
tests = TestList
    [ testAsIntegral
    , testIncrDecr
    , testNegate
    , testHalve
    ]

main = runTestTT tests
