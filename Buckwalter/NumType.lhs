Buckwalter.NumType -- Type level integers
Bjorn Buckwalter, bjorn.buckwalter@gmail.com
License: Public domain


= Summary =

This Module provides type level representations, hereafter referred
to as "NumTypes", of the (positive and negative) integers and some
basic operations (addition, subtraction...) on these. While functions
are provided for the operations NumTypes are solely for the type
level and their only value is 'undefined'.

There are similarities with the HNats of the HList library [1],
which was indeed a source of inspiration. Occasionally references
are made to the HNats. The main addition in this module is negative
numbers.


= Preliminaries =

This module requires GHC 6.6 or later. We utilize multi-parameter
type classes, phantom types, functional dependencies and undecidable
instances (and possibly additional unidentified GHC extensions).

> {-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

> module Buckwalter.NumType 
>   -- Basic classes.
>   ( NumType, PosType, NegType, NonZero
>   -- Arithmetic classes.
>   , Succ, Negate, Sum, Div, Mul
>   -- Functions.
>   , toIntegral, incr, decr, negate, (+), (-), (*), (/)
>   -- Data types.
>   , Zero, Pos, Neg
>   -- Type synonyms for convenience.
> 	, Pos1, Pos2, Pos3, Neg1, Neg2, Neg3
>   -- Values for convenience.
> 	, zero, pos1, pos2, pos3, pos4, pos5, neg1, neg2, neg3, neg4, neg5
>   ) where

> import Prelude hiding ((*), (/), (+), (-), negate) -- (undefined, Integral)
> import qualified Prelude as P ((+), (-))

Use the same fixity for operators as the Prelude.

> infixl 6  +, -


= NumTypes =

We start by defining a class encompassing all integers with the
class function 'toIntegral' that converts from the type-level to a
value-level 'Integral'.

> class NumType n where toIntegral :: (Integral a) => n -> a

Then we define classes encompassing all positive and negative
integers respectively. The 'PosType' class corresponds to HList's
'HNat'.  We also define a class for non-zero numbers (used to
prohibit division by zero).

> class (NumType n) => PosType n
> class (NumType n) => NegType n
> class (NumType n) => NonZero n

Now we use a trick from Oleg Kiselyov and Chung-chieh Shan [2]:

    -- The well-formedness condition, the kind predicate
    class Nat0 a where toInt :: a -> Int
    class Nat0 a => Nat a           -- (positive) naturals

    -- To prevent the user from adding new instances to Nat0 and especially
    -- to Nat (e.g., to prevent the user from adding the instance |Nat B0|)
    -- we do NOT export Nat0 and Nat. Rather, we export the following proxies.
    -- The proxies entail Nat and Nat0 and so can be used to add Nat and Nat0
    -- constraints in the signatures. However, all the constraints below
    -- are expressed in terms of Nat0 and Nat rather than proxies. Thus,
    -- even if the user adds new instances to proxies, it would not matter.
    -- Besides, because the following proxy instances are most general,
    -- one may not add further instances without overlapping instance extension.
    class    Nat0 n => Nat0E n
    instance Nat0 n => Nat0E n
    class    Nat n => NatE n
    instance Nat n => NatE n

We apply this trick to our classes.

> class    (NumType n) => NumTypeE n
> instance (NumType n) => NumTypeE n
> class    (PosType n) => PosTypeE n
> instance (PosType n) => PosTypeE n
> class    (NegType n) => NegTypeE n
> instance (NegType n) => NegTypeE n
> class    (NonZero n) => NonZeroE n
> instance (NonZero n) => NonZeroE n

TODO: actually change the export or internal instance names. I'm
inclined to change the internal names a retain the external ones.

We do not have to do this for our other classes. They have the above
classes in their constraints and since the instances are complete
(not proven) a new instance cannot be defined (actually used in the
case of GHC) without overlapping instances.

Now we Define the data types used to represent integers. We begin
with 'Zero', which we allow to be used as both a positive and a
negative number in the sense of the previously defined type classes.
'Zero' corresponds to HList's 'HZero'.

> data Zero
> instance NumType Zero where toIntegral _ = 0
> instance PosType Zero
> instance NegType Zero

Next we define the "successor" type, here called 'Pos' (corresponding
to HList's 'HSucc').

> data Pos n
> instance (PosType n) => NumType (Pos n) where 
>   toIntegral _ = toIntegral (undefined :: n) P.+ 1 
> instance (PosType n) => PosType (Pos n)
> instance (PosType n) => NonZero (Pos n)

We could be more restrictive using "data (PosType n) => Pos n" but
this constraint will not be checked (by GHC) anyway when 'Pos' is
used solely at the type level. 

Finally we define the "predecessor" type used to represent negative
numbers.

> data Neg n
> instance (NegType n) => NumType (Neg n) where
>   toIntegral _ = toIntegral (undefined :: n) P.- 1 
> instance (NegType n) => NegType (Neg n)
> instance (NegType n) => NonZero (Neg n)
 

= Show instances =

For convenience we create show instances for the defined NumTypes.

> instance Show Zero where show _ = "NumType 0"
> instance (PosType n) => Show (Pos n) where show x = "NumType " ++ show (toIntegral x)
> instance (NegType n) => Show (Neg n) where show x = "NumType " ++ show (toIntegral x)

 
= Negation, incrementing and decrementing =

We start off with some basic building blocks. Negation is a simple
matter of recursively changing 'Pos' to 'Neg' or vice versa while
leaving 'Zero' unchanged.

> class (NumType a, NumType b) => Negate a b | a -> b, b -> a

> instance Negate Zero Zero
> instance (PosType a, NegType b, Negate a b) => Negate (Pos a) (Neg b)
> instance (NegType a, PosType b, Negate a b) => Negate (Neg a) (Pos b) 

We define a type class for incrementing and decrementing NumTypes.
The 'incr' and 'decr' functions correspond roughly to HList's 'hSucc'
and 'hPred' respectively.

> class (NumType a, NumType b) => Succ a b | a -> b, b -> a

To increment NumTypes we either prepend 'Pos' to numbers greater
than or equal to Zero or remove a 'Neg' from numbers less than Zero.

> instance Succ Zero (Pos Zero)
> instance (PosType a) => Succ (Pos a) (Pos (Pos a))
> instance Succ (Neg Zero) Zero
> instance (NegType a) => Succ (Neg (Neg a)) (Neg a)


= Addition and subtraction =

Now let us move on towards more complex arithmetic operations. We
define classes for addition and subtraction of NumTypes.

> class (NumType a, NumType b, NumType c) 
>    => Sum a b c | a b -> c, a c -> b, b c -> a

In order to provide instances satisfying the functional dependencies
of 'Add' and 'Sub', in particular the property that any two parameters
uniquely define the third, we must use helper classes.

> class (NumType a, NumType b, NumType c) => Add' a b c | a b -> c

Adding anything to Zero gives "anything".

> instance (NumType a) => Add' Zero a a

When adding to a non-Zero number our strategy is to "transfer" type
constructors from the first type to the second type until the first
type is Zero. We use the 'incr' and 'decr' operators to do this.

> instance (PosType a, Succ b c, Add' a c d) => Add' (Pos a) b d
> instance (NegType a, Succ c b, Add' a c d) => Add' (Neg a) b d

We define our helper class for subtraction using negation and
addition.

> class (NumType a, NumType b, NumType c) => Sub' a b c | a b -> c
> instance (Negate b b', Add' a b' c) => Sub' a b c

Using the helper classes we can provide an instance of 'Add' that
satisfies its functional dependencies. We provide an instance of
'Sub' in terms of 'Add'.

> instance (Add' a b c, Sub' c b a) => Sum a b c


= Division =

We will do division on NumTypes before we do multiplication. This
may be surprising but it will in fact simplify the multiplication.
The reason for this is that we can have more a "reverse" functional
dependency for division but not for multiplication.  Consider the
expressions "x / y = z". If y and z are known we can always determine
x.  However, in "x * y = z" we can not determine x if y and z are
zero.

The 'NonZero' class is used as a constraint on the denominator 'b'
in our 'Div' class.

> class (NumType a, NonZero b, NumType c) => Div a b c | a b -> c, c b -> a

Zero divided by anything (we don't bother with infinity) equals
zero.

> instance (NonZero n) => Div Zero n Zero

Note that We could omit the NonZero class completely and instead
provide the following two instances.

] instance (PosType n) => Div Zero (Pos n) Zero
] instance (NegType n) => Div Zero (Neg n) Zero

Going beyond zero numbers we start with a base case with all numbers
positive.  We recursively subtract the denominator from nominator
while incrementing the result, until we reach the zero case.

> instance ( Sum n' (Pos n'') (Pos n)
>          , Div n'' (Pos n') n''', PosType n''') 
>       => Div (Pos n) (Pos n') (Pos n''')

Now we tackle cases with negative numbers involved. We trivially
convert these to the all-positive case and negate the result if
appropriate.

> instance ( NegType n, NegType n'
>          , Negate n p, Negate n' p'
>          , Div (Pos p) (Pos p') (Pos p''))
>       => Div (Neg n) (Neg n') (Pos p'')
> instance ( NegType n, Negate n p'
>          , Div (Pos p) (Pos p') (Pos p'')
>          , Negate (Pos p'') (Neg n''))
>       => Div (Pos p) (Neg n) (Neg n'')
> instance ( NegType n, Negate n p'
>          , Div (Pos p') (Pos p) (Pos p'')
>          , Negate (Pos p'') (Neg n''))
>       => Div (Neg n) (Pos p) (Neg n'')


= Multiplication =

Class for multiplication. Limited by the type checker stack. If the
multiplication is too large this error message will be emitted:

    Context reduction stack overflow; size = 20 
    Use -fcontext-stack=N to increase stack size to N

> class (NumType a, NumType b, NumType c) => Mul a b c | a b -> c

Providing instances for the 'Mul' class is really easy thanks to
the 'Div' class having the functional dependency "c b -> a".

> instance (NumType n) => Mul n Zero Zero
> instance (PosType p, Div c (Pos p) a) => Mul a (Pos p) c
> instance (NegType n, Div c (Neg n) a) => Mul a (Neg n) c


= Functions =

Based on the above type classes we can define functions for various
arithmetic operations on NumTypes. All functions are undefined and
only operate on the type level. Their main contribution is that
they permit NumType arithmetic without explicit (and tedious) type
declarations.

The main reason to collect all functions here is to keep the
preceeding sections free from distraction.

> negate :: (Negate a b) => a -> b
> negate _ = undefined

> incr :: (Succ a b) => a -> b
> incr _ = undefined
> decr :: (Succ a b) => b -> a
> decr _ = undefined

> (+) :: (Sum a b c) => a -> b -> c
> _ + _ = undefined
> (-) :: (Sum a b c) => c -> b -> a
> _ - _ = undefined

> (/) :: (Div a b c) => a -> b -> c 
> _ / _ = undefined

> (*) :: (Mul a b c) => a -> b -> c 
> _ * _ = undefined


= Convenince types and values =

Finally we define some type synonyms for the convenience of clients
of the library.

> type Pos1 = Pos Zero
> type Pos2 = Pos Pos1
> type Pos3 = Pos Pos2
> type Neg1 = Neg Zero
> type Neg2 = Neg Neg1
> type Neg3 = Neg Neg2

Analogously we also define some convenience values (all 'undefined'
but with the expected types).

> zero :: Zero  -- ~ hZero
> zero = undefined
> pos1 = incr zero
> pos2 = incr pos1
> pos3 = incr pos2
> pos4 = incr pos3
> pos5 = incr pos4
> neg1 = decr zero
> neg2 = decr neg1
> neg3 = decr neg2
> neg4 = decr neg3
> neg5 = decr neg4


= References =

[1] http://homepages.cwi.nl/~ralf/HList/
[2] http://okmij.org/ftp/Computation/resource-aware-prog/BinaryNumber.hs

