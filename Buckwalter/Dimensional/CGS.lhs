
*** EXPERIMENTAL ***

= Introduction =



> {-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

> module CGS where

> import Prelude ( undefined, Num, Fractional, Floating, Show )
> import qualified Prelude
> import Buckwalter.Dimensional hiding ( DLength, DMass, DTime, DElectricCurrent )
> import Buckwalter.Dimensional.Quantities
> import qualified Buckwalter.Dimensional.SIUnits as SI
> import qualified Buckwalter.NumType as N
> import Buckwalter.NumType ( Neg2, Neg1, Zero, Pos, Pos1, Pos2, Pos3, NumType )
> import Buckwalter.NumType ( neg2, neg1, zero, pos1, pos2, pos3 )
> import Data.Maybe (catMaybes)


= Dimensions =

Analogously with the SI we collect the base dimensions of the CGS
system in the data type 'CGSDim'.

> data CGSDim lh mh t

In the above 'lh' and 'mh' represent the number of half-exponents
of length and mass respectively while 't' represents the number of
whole-exponents. The base dimensions illustrate this.

> type DLength = CGSDim Pos2 Zero Zero
> type DMass   = CGSDim Zero Pos2 Zero
> type DTime   = CGSDim Zero Zero Pos1

(I'm ignorantly assuming that when working with the CGS (or MKS)
system you will only (meaningfully?) encounter half-exponents and
only of the length and mass dimensions. Of course, in other systems
other rational exponents may be encountered. Please correct me if
this assumption is wrong!)

We add a few other non-base dimensions for the sake of example.
Charge is particularly interesting as it illustrates the need for
half-exponents as described in [2].

> type DElectricCurrent = CGSDim Pos3 Pos1 Neg2
> type DCharge = CGSDim Pos3 Pos1 Neg1


= 'Mul', 'Div', 'Pow' and 'Root' instances =

The 'Mul', 'Div', 'Pow' and 'Root' instances are strictly analogous
with the SI.

> instance ( N.Sum lh lh' lh''
>          , N.Sum mh mh' mh''
>          , N.Sum t  t'  t'' ) => Mul (CGSDim lh   mh   t) 
>                                      (CGSDim lh'  mh'  t') 
>                                      (CGSDim lh'' mh'' t'')

> instance ( N.Sum lh lh' lh''
>          , N.Sum mh mh' mh''
>          , N.Sum t  t'  t'' ) => Div (CGSDim lh'' mh'' t'') 
>                                      (CGSDim lh'  mh'  t') 
>                                      (CGSDim lh   mh   t)

> instance ( N.Mul lh x lh'
>          , N.Mul mh x mh'
>          , N.Mul t  x t' ) => Pow (CGSDim lh  mh  t) x 
>                                   (CGSDim lh' mh' t')

> instance ( N.Div lh x lh'
>          , N.Div mh x mh'
>          , N.Div t  x t' ) => Root (CGSDim lh  mh  t) x 
>                                    (CGSDim lh' mh' t')


= Units =

We define the base units of the system. By defining 'meter' with a
"scale" of 100 we get a scale of one for 'centi meter'.

> meter  :: Num a => Unit DLength a
> meter  = Dimensional 100
> gram   :: Num a => Unit DMass a
> gram   = Dimensional 1
> second :: Num a => Unit DTime a
> second = Dimensional 1

We continue by defining the CGS equivalents of the other base SI units. Actually we limit ourselves to 'ampere' since I am not sure if or how the SI base dimensions other than current are expressed in CGS.

> ampere :: Floating a => Unit DElectricCurrent a
> ampere = prefix 3.33564e10 ((SI.centi meter ^ pos3) ^/ pos2 * gram ^/ pos2 * second ^ neg2)

We also define the preferred CGS unit for charge.

> franklin :: Floating a => Unit DCharge a
> franklin = gram ^/ pos2 * (SI.centi meter ^ pos3) ^/ pos2 / second


= Conversion from SI =

In order to convert a 'Quantity' from the SI system to the CGS
system we use the strategy of dividing the quantity by the SI base
unit and multiplying the resulting number (sans dimension) by the
equivalent CGS unit. In order to realize this strategy we must be
able to obtain the SI base unit and the equivalent CGS unit for a
given quantity. We start with the SI unit since it is trivial.

> unit_SI :: Num a => Quantity (Dim l m t i th n j) a -> Unit (Dim l m t i th n j) a
> unit_SI _ = Dimensional 1


> -- Get the SI base unit of a Quantity.
> unit_SI' :: forall a l m t i th n j.
>         ( Fractional a
>         , N.Mul Zero l  Zero, N.Mul Pos1 l  l
>         , N.Mul Zero m  Zero, N.Mul Pos1 m  m
>         , N.Mul Zero t  Zero, N.Mul Pos1 t  t
>         , N.Mul Zero i  Zero, N.Mul Pos1 i  i
>         , N.Mul Zero th Zero, N.Mul Pos1 th th
>         , N.Mul Zero n  Zero, N.Mul Pos1 n  n
>         , N.Mul Zero j  Zero, N.Mul Pos1 j  j
>         , N.Sum l  Zero l
>         , N.Sum Zero m  m,  N.Sum m  Zero m
>         , N.Sum Zero t  t,  N.Sum t  Zero t
>         , N.Sum Zero i  i,  N.Sum i  Zero i
>         , N.Sum Zero th th, N.Sum th Zero th
>         , N.Sum Zero n  n,  N.Sum n  Zero n
>         , N.Sum Zero j  j
>         ) => Quantity (Dim l m t i th n j) a -> Unit (Dim l m t i th n j) a
> unit_SI' _ = SI.meter        ^ (undefined :: l)
>           * SI.kilo SI.gram ^ (undefined :: m)
>           * SI.second       ^ (undefined :: t)
>           * SI.ampere       ^ (undefined :: i)
>           * SI.kelvin       ^ (undefined :: th)
>           * SI.mole         ^ (undefined :: n)
>           * SI.candela      ^ (undefined :: j)

> -- Alternate definition (this won't work for other than SI).

> -- Get the CGS unit corresponding to the SI base unit of a Quantity.
> unit_CGS :: forall a l m t i l2 m2 il it l' m' t'.
>          ( Floating a
>          , N.Mul Zero l Zero, N.Mul Pos2 l l2
>          , N.Mul Zero m Zero, N.Mul Pos2 m m2
>          , N.Mul Zero t Zero, N.Mul Pos1 t t
>          , N.Sum l2 Zero l2
>          , N.Sum Zero m2 m2,  N.Sum m2 Zero m2
>          , N.Sum Zero t  t
>          , N.Mul Pos3 i  il
>          , N.Mul Pos1 i  i
>          , N.Mul Neg2 i  it
>          , N.Sum l2 il l'
>          , N.Sum m2 i  m'
>          , N.Sum t  it t'
>          ) => Quantity (Dim l m t i Zero Zero Zero) a -> Unit (CGSDim l' m' t') a
> unit_CGS _ = meter        ^ (undefined :: l)
>            * SI.kilo gram ^ (undefined :: m)
>            * second       ^ (undefined :: t)
>            * ampere       ^ (undefined :: i)

> -- {-
> fromSI x = x /~ unit_SI'  x *~ unit_CGS x
> -- toSI   x = x /~ unit_CGS x *~ unit_SI  x

> toSI :: forall a l m t i l2 m2 il it l' m' t'.
>          ( Floating a
>          , N.Mul Zero l Zero, N.Mul Pos2 l l2
>          , N.Mul Zero m Zero, N.Mul Pos2 m m2
>          , N.Mul Zero t Zero, N.Mul Pos1 t t
>          , N.Sum l2 Zero l2
>          , N.Sum Zero m2 m2,  N.Sum m2 Zero m2
>          , N.Sum Zero t  t
>          , N.Mul Pos3 i  il
>          , N.Mul Pos1 i  i
>          , N.Mul Neg2 i  it
>          , N.Sum l2 il l'
>          , N.Sum m2 i  m'
>          , N.Sum t  it t'
>          ) => Quantity (CGSDim l' m' t') a -> Quantity (Dim l m t i Zero Zero Zero) a
> toSI x = x /~ unit_CGS (undefined :: Quantity (Dim l m t i Zero Zero Zero) a) *~ unit_SI' (undefined :: Quantity (Dim l m t i Zero Zero Zero) a)


> -- -}

> class SIEquivalent a d d' | d -> d' where -- 'd' is the SI dimension.
>         unit :: Dimensional v d a -> Unit d' a

> instance ( Floating a
>          , N.Mul Zero l Zero, N.Mul Pos2 l l2
>          , N.Mul Zero m Zero, N.Mul Pos2 m m2
>          , N.Mul Zero t Zero, N.Mul Pos1 t t
>          , N.Sum l2 Zero l2
>          , N.Sum Zero m2 m2,  N.Sum m2 Zero m2
>          , N.Sum Zero t  t
>          , N.Mul Pos3 i  il
>          , N.Mul Pos1 i  i
>          , N.Mul Neg2 i  it
>          , N.Sum l2 il l'
>          , N.Sum m2 i  m'
>          , N.Sum t  it t'
>          ) => SIEquivalent a (Dim l m t i Zero Zero Zero) (CGSDim l' m' t') where
>             unit _ = meter ^ (undefined :: l)
>                    * SI.kilo gram ^ (undefined :: m)
>                    * second       ^ (undefined :: t)
>                    * ampere       ^ (undefined :: i)

> toSI' :: ( SIEquivalent a (Dim l m t i Zero Zero Zero) (CGSDim l' m' t') ) => Quantity (CGSDim l' m' t') a -> Quantity (Dim l m t i Zero Zero Zero) a
> toSI' x = x /~ unit    (undefined :: Quantity (Dim l m t i Zero Zero Zero) a)
>             *~ unit_SI (undefined :: Quantity (Dim l m t i Zero Zero Zero) a)


> {-
> unit_CGS' :: ( Floating a, SIEquivalent (Dim l m t i Zero Zero Zero) (CGSDim l' m' t') )
>           => Quantity (Dim l m t i Zero Zero Zero) a -> Unit (CGSDim l' m' t') a
> unit_CGS' _ = meter        ^ (undefined :: l)
>             * SI.kilo gram ^ (undefined :: m)
>             * second       ^ (undefined :: t)
>             * ampere       ^ (undefined :: i)
> -}


= 'Show' instance =

Out of lazyness we use the notation "sqrt(cm)" to represent halves
of integral dimensions. Nothing is technically keeping us from doing
a better job here.

> instance forall lh mh t.
>     ( NumType lh
>     , NumType mh
>     , NumType t
>     ) => Show (CGSDim lh mh t) where
>     show _ = (Prelude.unwords Prelude.. catMaybes)
>              [ dimUnit "sqrt(cm)" (undefined :: lh)
>              , dimUnit "sqrt(g)"  (undefined :: mh)
>              , dimUnit "s"        (undefined :: t)
>              ]


= References =

[1] Chuck's email
[2] http://www.tf.uni-kiel.de/matwis/amat/mw1_ge/kap_2/basics/b2_1_14.html
