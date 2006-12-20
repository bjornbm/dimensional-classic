Buckwalter.NumType -- Type level integers
Bjorn Buckwalter, bjorn@buckwalter.se
2006-12-19, version 0.1 (experimental)
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

> module Buckwalter.NumType (
> 	Zero, Pos, Neg,
> 	NumType, PosType, NegType,
> 	Negate, negate, Incr, incr, Decr, decr, 
>   Add, (+), Sub, (-), Halve, halve,
> 	Pos1, Pos2, Pos3, Neg1, Neg2, Neg3,
> 	zero, pos1, pos2, pos3, neg1, neg2, neg3,
>   ) where

> import Prelude (undefined)

Use the same fixity for operators as the Prelude.

> infixl 6  +, -


= NumTypes =

We start by defining a class encompassing all integers.

> class NumType n

Then we define classes encompassing all positive and negative integers
respectively. The 'PosType' class corresponds to HList's 'HNat'.

> class NumType n => PosType n
> class NumType n => NegType n

Now we Define the data types used to represent integers. We begin
with 'Zero' which we allow to be used as both a positive and a
negative number in the sense of the previously defined type classes.
'Zero' corresponds to HList's 'HZero'.

> data Zero
> instance NumType Zero
> instance PosType Zero
> instance NegType Zero

Next we define the "successor" type, here called 'Pos' (corresponding
to HList's 'HSucc').

> data Pos n
> instance (PosType n) => NumType (Pos n)
> instance (PosType n) => PosType (Pos n)

We could be more restrictive using "data (PosType n) => Pos n" but
this constriant will not be checked (by GHC) anyway when 'Pos' is
used solely at the type level. Also note that in the functions of
the remainder of this module we have neglected to provide e.g.
'NumType' or 'PosType' constraints, perhaps out of lazyness more
than anything else.

Finally we define the "predecessor" type used to represent negative
numbers.

> data Neg n
> instance (NegType n) => NumType (Neg n)
> instance (NegType n) => NegType (Neg n)
 
 
= NumType arithmetic =

We start off with some basic building blocks. Negation is a simple
matter of recursively changing 'Pos' to 'Neg' or vice versa while
leaving 'Zero' unchanged.

> class (NumType a, NumType b) => Negate a b | a -> b 
>   where negate :: a -> b
> instance Negate Zero Zero where negate = undefined
> instance (PosType a, NegType b, Negate a b) => Negate (Pos a) (Neg b)
>    where negate _ = undefined
> instance (NegType a, PosType b, Negate a b) => Negate (Neg a) (Pos b) 
> 	where negate _ = undefined

To increment NumTypes we either prepend 'Pos' to numbers greater
than or equal to Zero or remove a 'Neg' from numbers less than Zero.
The 'incr' function corresponds roughly to HList's 'hSucc'.

> class (NumType a, NumType b) => Incr a b | a -> b where incr :: a -> b
> instance Incr Zero (Pos Zero) where incr _ = undefined
> instance (PosType a) => Incr (Pos a) (Pos (Pos a)) where incr _ = undefined
> instance (NegType a) => Incr (Neg a) a where incr _ = undefined

Decrementing is similar but we either remove a 'Pos' from numbers
greater than Zero or prepend a 'Neg' to numbers less than or equal
to 'Zero'. The 'decr' function corresponds roughly to HList's
'hPred'.

> class (NumType a, NumType b) => Decr a b | a -> b where decr :: a -> b
> instance Decr Zero (Neg Zero) where decr _ = undefined
> instance (PosType a) => Decr (Pos a) a where decr _ = undefined
> instance (NegType a) => Decr (Neg a) (Neg (Neg a)) where decr _ = undefined

We define a class for addition of NumTypes.

> class (NumType a, NumType b, NumType c) => Add a b c | a b -> c
> 	where (+) :: a -> b -> c

Adding anything to Zero gives "anything".

> instance (NumType a) => Add Zero a a where (+) _ = undefined

When adding to a non-Zero number our strategy is to "transfer" type
constructors from the first type to the second type until the first
type is Zero. We use the 'incr' and 'decr' operators to do this.

> instance (PosType a, Incr b c, Add a c d) => Add (Pos a) b d
>   where _ + _ = undefined
> instance (NegType a, Decr b c, Add a c d) => Add (Neg a) b d
>   where _ + _ = undefined

We define subtraction using negation and addition.

> class (NumType a, NumType b, NumType c) => Sub a b c | a b -> c 
> 	where (-) :: a -> b -> c
> instance (Negate b b', Add a b' c) => Sub a b c where _ - _ = undefined

We neglect to provide multiplication and division. However, let us
define a halving operator which is useful when using a NumType to
represent an power. Taking the square root requires halving the
power.

> class (NumType a, NumType b) => Halve a b | a -> b where halve :: a -> b
> instance Halve Zero Zero where halve _ = undefined
> instance (PosType a, PosType b, Halve a b) => Halve (Pos (Pos a)) (Pos b) 
> 	where halve _ = undefined
> instance (NegType a, NegType b, Halve a b) => Halve (Neg (Neg a)) (Neg b) 
> 	where halve _ = undefined


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
> neg1 = decr zero
> neg2 = decr neg1
> neg3 = decr neg2


= References =

[1] http://homepages.cwi.nl/~ralf/HList/

