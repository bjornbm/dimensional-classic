Buckwalter.Dimensional.Extensible -- Extensible physical dimensions
Bjorn Buckwalter, bjorn.buckwalter@gmail.com
License: BSD3


= Summary =

On January 3 Mike Gunter asked[1]:

| The very nice Buckwalter and Denney dimensional-numbers packages
| both work on a fixed set of base dimensions.  This is a significant
| restriction for me--I want to avoid adding apples to oranges as
| well as avoiding adding meters to grams.  Is it possible to have
| an extensible set of base dimensions?  If so, how usable can such
| a system be made?  Is it very much worse than a system with a fixed
| set of base dimensions?

In this module we facilitate the addition an arbitrary number of
"extra" dimensions to the seven base dimensions defined in
'Buckwalter.Dimensional'. A quantity or unit with one or more extra
dimensions will be referred to as an "extended dimensional".


= Preliminaries =

Similarly with 'Buckwalter.Dimensional' this module requires GHC
6.6 or later.

> {-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

> module Buckwalter.Dimensional.Extensible
>   where

> import Prelude hiding
>   ((*), (/), (+), (-), (^), sqrt, negate, pi, sin, cos, exp)
> import qualified Prelude as P ((*), (/), sin, cos, exp)
> import Buckwalter.NumType (NumType, Add, Sub, Halve, Negate, Zero, Pos, Neg) 
> import Buckwalter.Dimensional hiding ((/~), square, cubic, sin, cos, exp)


= DExt, Apples and Oranges =

We define the datatype 'DExt' which we will use to increase the
number of dimensions from the seven SI base dimensions to an arbitrary
number of dimensions. We make 'DExt' an instance of 'Dims' allowing
us to use the 'Dimensional' type without change.

> data (NumType n, Dims d) => DExt n d
> instance Dims (DExt n d)

Using 'DExt' we can define type synonyms for extended dimensions
applicable to our problem domain. For example, Mike Gunter could
define the 'Apples' and 'Oranges' dimensions and the corresponding
quantities.

] type DApples  = DExt Pos1 (DExt Zero DOne)
] type DOranges = DExt Zero (DExt Pos1 DOne)

] type Apples   = Quantity DApples
] type Oranges  = Quantity DOranges

And while he was at it he could define corresponding units.

] apple  :: Num a => Unit DApples a
] apple  = Dimensional 1
] orange :: Num a => Unit DOranges a
] orange = Dimensional 1


= Arithmetic =

We get negation, addition and subtraction for free with extended
dimensionals. However, we will need instances of the 'Mul', 'Div'
and 'Sqrt' classes for the corresponding operations to work.

> instance (Add n n' n'', Mul d d' d'') 
>       => Mul (DExt n d) (DExt n' d') (DExt n'' d'')

> instance (Sub n n' n'', Div d d' d'') 
>       => Div (DExt n d) (DExt n' d') (DExt n'' d'')

> instance (Halve n n', Sqrt d d') => Sqrt (DExt n d) (DExt n' d')

Now, in order to work seamlessly with the quantities and units
defined in 'Buckwalter.Dimensional' we must be able to automatically
extend their dimensions when multiplying or dividing by an extended
dimensional.

> instance (Mul d (Dim l m t i th n j) d') => Mul (DExt x d)
>                                                 (Dim l m t i th n j)
>                                                 (DExt x d')
> instance (Mul (Dim l m t i th n j) d d') => Mul (Dim l m t i th n j)
>                                                 (DExt x d)
>                                                 (DExt x d')

> instance (Div d (Dim l m t i th n j) d') => Div (DExt x d)
>                                                 (Dim l m t i th n j)
>                                                 (DExt x d')
> instance (Div (Dim l m t i th n j) d d', Negate x x') 
>       => Div (Dim l m t i th n j)
>              (DExt x d) 
>              (DExt x' d')

The above instances also enable taking integer powers of extended
dimensionals. To allow dimensionless quantities to be raised to the
power of any other dimensionless quantity (where either or both of
the quantities may be an extended dimensional) some new 'Power'
instances are required.

> instance (Floating a, Power a d (Dimensionless a) DOne)
>       => Power a (DExt Zero d) (Dimensionless a) DOne
>   where (Dimensional x) ^ (Dimensional y) = Dimensional (x ** y)

> instance (Floating a, Power a DOne (Quantity d a) DOne)
>       => Power a DOne (Quantity (DExt Zero d) a) DOne
>   where (Dimensional x) ^ (Dimensional y) = Dimensional (x ** y)

> instance (Floating a, Power a d (Quantity d' a) DOne)
>       => Power a (DExt Zero d) (Quantity (DExt Zero d') a) DOne
>   where (Dimensional x) ^ (Dimensional y) = Dimensional (x ** y)

Note that the extra dimensions are dropped from the result. This
is acceptable since they will be added again as necessary when
multiplying or dividing by extended dimensionals.


= Elementary functions, 'square' and 'cubic' =

In 'Buckwalter.Dimensional' the elementary functions where restricted
to dimensionals with the physical dimension 'DOne'. The same applies
to the 'square' and 'cubic' functions and the 'DLength' dimension
respectively. Unfortunately this implementation is inflexible in
that it will not accommodate extended dimensionals even if they have
no extend into the extra dimensions (i.e. the powers are all 'Zero').
To solve this problem we must provide new, generalized implementations
of such functions.

We start by defining the class 'BaseDim' which relates extended
dimensions to the corresponding base dimension if applicable. An
extended dimension has a corresponding base dimension if the powers
of all extra dimensions are 'Zero'. Two 'BaseDim' instances are
required.

> class (Dims d, Dims d') => BaseDim d d' | d -> d'
> instance BaseDim (Dim l m t i th n j) (Dim l m t i th n j)
> instance (BaseDim d d') => BaseDim (DExt Zero d) d'

Using 'BaseDim' in the constraints we can now redefine the elementary
functions. As with the 'Power' instances above we drop the extra
dimensions from the results.

> sin, cos :: (Floating a, BaseDim d DOne) => Quantity d a -> Dimensionless a
> sin (Dimensional x) = Dimensional (P.sin x)
> cos (Dimensional x) = Dimensional (P.cos x)

> exp :: (Floating a, BaseDim d DOne) => Quantity d a -> Dimensionless a
> exp (Dimensional x) = Dimensional (P.exp x)

Ditto for 'square' and 'cubic'.

> square :: (Num a, BaseDim d DLength) => Unit d a -> Unit DArea a
> square (Dimensional x) = Dimensional (x P.* x)
> cubic  :: (Num a, BaseDim d DLength) => Unit d a -> Unit DVolume a
> cubic  (Dimensional x) = Dimensional (x P.* x P.* x)

(As a side note we could probably use 'BaseDim' to clean up the
constraints and/or reduce the instances of 'Power' above, and perhaps
for 'Mul' and 'Div'.)


= Two days later... =

I realized that '(/~)' was broken for the combination of extended
quantity and "base" units.  Specifically the compiler would choke
when dividing a extended quantity by a base unit even if the
dimensions are equivalent.  To work around this we define the type
class 'Minimal' which relates 'Dims' to the minimal equivalent
'Dims' type.

> class (Dims d, Dims d') => Minimal d d' | d -> d'
> instance Minimal (Dim l m t i th n j) (Dim l m t i th n j)
> instance (Minimal d d') => Minimal (DExt Zero d) d'
> instance Minimal (DExt (Pos n) d) (DExt (Pos n) d)
> instance Minimal (DExt (Neg n) d) (DExt (Neg n) d)

We then redefine the '(/~)' operator.

> infixl 7  /~
> (/~) :: (Fractional a, Minimal d d'', Minimal d' d'') 
>      => Quantity d a -> Unit d' a -> a
> Dimensional x /~ Dimensional y = x P./ y


= References =

[1] http://www.haskell.org/pipermail/haskell-cafe/2007-January/021069.html

