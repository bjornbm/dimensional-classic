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
> import qualified Prelude as P ((*), (/), (+), (-), sin, cos, exp)
> import Buckwalter.NumType (NumType, Add, Sub, Halve, Negate, Zero, Pos, Neg) 
> import Buckwalter.Dimensional hiding ((/~), (+), (-), square, cubic, sin, cos, exp)


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

] type DApples  = DExt Pos1 DOne
] type DOranges = DExt Zero (DExt Pos1 DOne)

] type Apples   = Quantity DApples
] type Oranges  = Quantity DOranges

And while he was at it he could define corresponding units.

] apple  :: Num a => Unit DApples a
] apple  = Dimensional 1
] orange :: Num a => Unit DOranges a
] orange = Dimensional 1


= Minimal representation =

Extended dimensions are added in a left to right (or outermost to
innermost) with the base dimensions ('Dim') terminating the sequence.
Any dimensions with zero extent directly preceeding the 'Dim' are
insignificant. A /minimal representation/ of a dimension is one
where all insignificant dimensions have been removed. For example,
the minimal representation of "DExt Pos1 (DExt Zero (Dim ...))" is
"DExt Pos1 (Dim ...)". 

We define the type class 'Minimal' that "derives" the minimal
representation of a dimension.

 > class (Dims d, Dims d') => Minimal d d' | d -> d'
 > instance Minimal (Dim l m t i th n j) (Dim l m t i th n j)
 > instance (Minimal d d') => Minimal (DExt (Pos n) d) (DExt (Pos n) d')
 > instance (Minimal d d') => Minimal (DExt (Neg n) d) (DExt (Neg n) d')
 > instance (Minimal d d', Minimal' d' d'') => Minimal (DExt Zero d) d''

For the last instance we need the helping 'Minimal'' class. If the
first parameter ('d') of the 'Minimal'' class is a 'Dim' then the
second parameter is a 'Dim'. Otherwise the first parameter is
prefixed by a zero dimension. The intent of this class is to obtain
the minimal representation of "DExt Zero d", assuming d is already
minimal.

 > class (Dims d, Dims d') => Minimal' d d' | d -> d'
 > instance Minimal' (Dim l m t i th n j) (Dim l m t i th n j)
 > instance Minimal' (DExt n d) (DExt Zero (DExt n d))

> class (Dims d, Dims d') => Minimal d d' | d -> d'
> instance Minimal (DExt (Pos n) d) (DExt (Pos n) d)
> instance Minimal (DExt (Neg n) d) (DExt (Neg n) d)
> instance Minimal (DExt Zero (DExt n d)) (DExt Zero (DExt n d))
> instance Minimal (DExt Zero (Dim l m t i th n j)) (Dim l m t i th j j)

The minimal representation should always be
used, as in the above example.


= Mul and Div = 

We get negation, addition and subtraction for free with extended
dimensionals. However, we will need instances of the 'Mul', 'Div'
and 'Sqrt' classes for the corresponding operations to work.


Multiplication and division can cause dimensions to be eliminated.
We use the 'Minimal' type class to guarantee that the result of a
multiplication or division has a minimal representation.

When only one of the 'Mul' factors is an extended dimensional there is
no need to minimize.

> instance (Mul d (Dim l m t i th n j) d') 
>       => Mul (DExt x d) (Dim l m t i th n j) (DExt x d')
> instance (Mul (Dim l m t i th n j) d d') 
>       => Mul (Dim l m t i th n j) (DExt x d) (DExt x d')

If both of the factors are extended the product must be minimized.

> instance (Add n n' n'', Mul d d' d'', Minimal (DExt n'' d'') d''') 
>       => Mul (DExt n d) (DExt n' d') d'''

Analogously for 'Div'.

> instance (Div d (Dim l m t i th n j) d') 
>       => Div (DExt x d) (Dim l m t i th n j) (DExt x d')
> instance (Div (Dim l m t i th n j) d d', Negate x x') 
>       => Div (Dim l m t i th n j) (DExt x d) (DExt x' d')

> instance (Sub n n' n'', Div d d' d'', Minimal (DExt n'' d'') d''') 
>       => Div (DExt n d) (DExt n' d') d'''

The 'sqrt' function is straight-forward since it cannot eliminate
any dimensions.

> instance (Halve n n', Sqrt d d') => Sqrt (DExt n d) (DExt n' d')


= References =

[1] http://www.haskell.org/pipermail/haskell-cafe/2007-January/021069.html

