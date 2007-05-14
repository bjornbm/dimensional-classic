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
dimensions will be referred to as an "extended Dimensional".


= Preliminaries =

Similarly with 'Buckwalter.Dimensional' this module requires GHC
6.6 or later.

> {-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

> module Buckwalter.Dimensional.Extensible (DExt) where

> import Buckwalter.Dimensional ( Dim, Mul, Div, Power, Root )
> import Buckwalter.NumType ( NumType, Sum, Negate, Zero, Pos, Neg ) 
> import qualified Buckwalter.NumType as N ( Div, Mul )


= 'DExt', 'Apples' and 'Oranges' =

We define the datatype 'DExt' which we will use to increase the
number of dimensions from the seven SI base dimensions to an arbitrary
number of dimensions.

> data DExt n d

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

When extending dimensions we adopt the convention that the first
(outermost) dimension is the reference for aligning dimensions, as
shown in the above example. This is important when performing
operations on two Dimensionals with a differing number of extended
dimensions.


= The 'DropZero' class =

The choice of convention may seem backwards considering the opposite
convention is used for NumTypes (though for NumTypes the distinction
is arguably irrelevant). However, this choice facilitates relatively
simple interoperability with base dimensions. In particular it lets
us drop any dimensions with zero extent adjacent to the terminating
'Dim'. To capture this property we define the 'DropZero' class.

> class DropZero d d' | d -> d'

The following 'DropZero' instances say that when an extended dimension
with zero extent is next to a 'Dim' the extended dimension can be
dropped. In all other cases the dimensions are retained as is.

> instance DropZero (DExt Zero (Dim l m t i th n j)) (Dim l m t i th j j)
> instance DropZero (DExt Zero (DExt n d)) (DExt Zero (DExt n d))
> instance DropZero (DExt (Pos n) d) (DExt (Pos n) d)
> instance DropZero (DExt (Neg n) d) (DExt (Neg n) d)


= Classes from 'Buckwalter.Dimensional' = 

We get negation, addition and subtraction for free with extended
Dimensionals. However, we will need instances of the 'Mul', 'Div',
'Power' and 'Root' classes for the corresponding operations to work.

Multiplication and division can cause dimensions to be eliminated.
We use the 'DropZero' type class to guarantee that the result of a
multiplication or division has a minimal representation.

When only one of the 'Mul' factors is an extended dimensional there is
no need to minimize.

> instance (Mul d (Dim l m t i th n j) d') 
>       => Mul (DExt x d) (Dim l m t i th n j) (DExt x d')
> instance (Mul (Dim l m t i th n j) d d') 
>       => Mul (Dim l m t i th n j) (DExt x d) (DExt x d')

If both of the factors are extended the product must be minimized.

> instance (Sum n n' n'', Mul d d' d'', DropZero (DExt n'' d'') d''') 
>       => Mul (DExt n d) (DExt n' d') d'''

Analogously for 'Div'.

> instance (Div d (Dim l m t i th n j) d') 
>       => Div (DExt x d) (Dim l m t i th n j) (DExt x d')
> instance (Div (Dim l m t i th n j) d d', Negate x x') 
>       => Div (Dim l m t i th n j) (DExt x d) (DExt x' d')

> instance (Sum n'' n' n, Div d d' d'', DropZero (DExt n'' d'') d''') 
>       => Div (DExt n d) (DExt n' d') d'''

The instances for 'Power' and 'Root' are simpler since they can not
change any previously non-zero to be eliminated.

> instance (N.Mul n x n', Power d x d') => Power (DExt n d) x (DExt n' d')
> instance (N.Div n x n', Root  d x d') => Root  (DExt n d) x (DExt n' d')


= References =

[1] http://www.haskell.org/pipermail/haskell-cafe/2007-January/021069.html

