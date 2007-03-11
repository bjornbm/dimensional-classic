{-# OPTIONS_GHC -fno-monomorphism-restriction #-}

module NumTypeTests where

import Buckwalter.NumType
import Prelude hiding ((*), (/), (+), (-), negate) -- (undefined, Integral)
import qualified Prelude as P ((*), (/), (+), (-), negate)
import Test.HUnit


-- Compares a type level unary function with a value level unary function 
-- by converting 'NumType' to 'Integral'. This assumes that the 'asIntegral'
-- function is solid.
unaryTest :: (NumType n, NumType n', Num a) 
          => (n -> n') -> (a -> a) -> n -> Test
unaryTest f f' x = TestCase $ assertEqual 
    "Unary function Integral equivalence" 
    (f' ((fromIntegral . asIntegral) x)) 
    ((fromIntegral . asIntegral) (f x))

-- Compares a type level binary function with a value level binary function 
-- by converting 'NumType' to 'Integral'. This assumes that the 'asIntegral'
-- function is solid.
binaryTest :: (NumType n, NumType n', NumType n'', Num a) 
           => (n -> n' -> n'') -> (a -> a -> a) -> n -> n' -> Test
binaryTest f f' x y = TestCase $ assertEqual 
    "Binary function Integral equivalence" 
    (f' ((fromIntegral . asIntegral) x) ((fromIntegral . asIntegral) y)) 
    ((fromIntegral . asIntegral) (f x y))

-- Test that conversion to 'Integral' works as expected.
testAsIntegral = TestLabel "Integral equivalence tests" $ TestList 
    [ TestCase $ -2 @=? asIntegral neg2
    , TestCase $ -1 @=? asIntegral neg1
    , TestCase $  0 @=? asIntegral zero
    , TestCase $  1 @=? asIntegral pos1
    , TestCase $  2 @=? asIntegral pos2
    ] -- By induction all other NumTypes should be good if these are.

-- Test increment and decrement for a bunch of 'NumTypes'.
testIncrDecr = TestLabel "Increment and decrement tests" $ TestList
    [ t neg2
    , t neg1
    , t zero
    , t pos1
    , t pos1
    ] where 
        t x = TestList [ unaryTest incr (P.+ 1) x
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

-- Test halving.
testHalve = TestLabel "Halving tests" $ TestList
    [ unaryTest halve (P./ 2) neg4
    , unaryTest halve (P./ 2) neg2
    , unaryTest halve (P./ 2) zero
    , unaryTest halve (P./ 2) neg2
    , unaryTest halve (P./ 2) neg4
    ]

-- Test addition.
testAddition = TestLabel "Addition tests" $ TestList
    [ binaryTest (+) (P.+) pos2 pos3
    , binaryTest (+) (P.+) neg2 pos3
    , binaryTest (+) (P.+) pos2 neg3
    , binaryTest (+) (P.+) neg2 neg3
    ]

-- Test subtraction.
testSubtraction = TestLabel "Subtraction tests" $ TestList
    [ binaryTest (-) (P.-) pos2 pos5
    , binaryTest (-) (P.-) neg2 pos5
    , binaryTest (-) (P.-) pos2 neg5
    , binaryTest (-) (P.-) neg2 neg5
    ]

-- Test multiplication.
testMultiplication = TestLabel "Multiplication tests" $ TestList
    [ binaryTest (*) (P.*) pos2 pos5
    , binaryTest (*) (P.*) neg2 pos5
    , binaryTest (*) (P.*) pos2 neg5
    , binaryTest (*) (P.*) neg2 neg5
    , binaryTest (*) (P.*) pos2 zero
    , binaryTest (*) (P.*) neg2 zero
    , binaryTest (*) (P.*) zero pos5
    , binaryTest (*) (P.*) zero neg5
    ]

-- Test division.
testDivision = TestLabel "Division tests" $ TestList
    [ binaryTest (/) (P./) pos4 pos2
    , binaryTest (/) (P./) zero pos5
    --, binaryTest (/) (P./) neg4 pos2
    --, binaryTest (/) (P./) pos4 neg2
    --, binaryTest (/) (P./) neg4 neg2
    ]


-- Collect the test cases.
tests = TestList
    [ testAsIntegral
    , testIncrDecr
    , testNegate
    , testHalve
    , testAddition
    , testSubtraction
    , testMultiplication
    ]

main = runTestTT tests