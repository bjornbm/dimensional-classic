Numeric.Dimensional.CGS -- CGS system of units
Bjorn Buckwalter, bjorn.buckwalter@gmail.com
License: BSD3

*** EXPERIMENTAL ***


= Introduction =

This module was prompted by an email from Chuck Blake[1]. He asked if
the Dimensional library could support other systems of units than
SI, in particular systems such as the centimeter-gram-second (CGS)
system where fractional exponents of dimensions occur. He also
wondered whether it was possible to convert quantities between
different systems while statically ensuring that a given conversion
was valid.

In this module we show that we can in a straight forward manner
support systems with rational exponents, provided that the rationals
that may be encountered are known a priori. As an example we provide
a rudimentary implementation of the CGS system. 

We also show that we can indeed statically prohibit invalid conversions
between different systems.


= Caveats =

I'm ignorantly assuming that when working with the CGS (or MKS)
system you will only (meaningfully?) encounter half-exponents and
only of the length and mass dimensions. Of course, in other systems
other rational exponents may be encountered.

I am also assuming that the CGS system would not be employed when
working with temperature, amount or luminosity. This is evident in
the below type signatures where I have assumed zero extent in the
temperature, amount and luminosity dimensions. If this is incorrect
I would appreciate pointers to the CGS representation of these
dimensions.

Please correct and inform me if my assumptions are wrong! 


= Preliminaries =

> {-# LANGUAGE UndecidableInstances, ScopedTypeVariables #-}

> module Numeric.Units.Dimensional.CGS where

> import Prelude ( undefined, Num, Fractional, Floating, Show, recip, Double )
> import qualified Prelude
> import Numeric.Units.Dimensional hiding ( DLength, DMass, DTime, DElectricCurrent )
> import Numeric.Units.Dimensional.Quantities as SIQ
> import qualified Numeric.Units.Dimensional.SIUnits as SI
> import qualified Numeric.NumType as N
> import Numeric.NumType ( Neg2, Neg1, Zero, Pos, Pos1, Pos2, Pos3, NumType )
> import Numeric.NumType ( neg2, neg1, zero, pos1, pos2, pos3 )
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

We add a few non-base dimensions for the sake of example. Charge
is particularly interesting as it illustrates the need for
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

We continue by defining the CGS equivalents of the other base SI
units. Actually we limit ourselves to 'ampere' since I am not sure
if or how the SI base dimensions other than current are expressed
in CGS.

> ampere :: Floating a => Unit DElectricCurrent a
> ampere = prefix (recip 3.33564e-10) ((SI.centi meter ^ pos3) ^/ pos2 * gram ^/ pos2 * second ^ neg2)

We also define the preferred CGS unit for charge.

> franklin :: Floating a => Unit DCharge a -- Also known as "esu".
> franklin = gram ^/ pos2 * (SI.centi meter ^ pos3) ^/ pos2 / second


= Conversion from SI =

At some point we may wish to convert an SI quantity to a CGS quantity
or vice versa.

In order to convert a 'Quantity' from the SI system to the CGS
system we use the strategy of dividing the quantity by the SI base
unit and multiplying the resulting number (sans dimension) by the
equivalent CGS unit. To realize this strategy we must be able to
obtain the SI base unit and the equivalent CGS unit for a given
quantity. We start with the SI unit since it is trivial.

> unit_SI :: Num a => Quantity (Dim l m t i th n j) a -> Unit (Dim l m t i th n j) a
> unit_SI _ = Dimensional 1

(Perhaps the above function would be better defined in another
module.)

Obtaining the CGS unit corresponding to the SI base unit of a
Quantity isn't quite as trivial. The function body itself is
straight-forward enough, the hairy part is the type signature.

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

Note that since the base dimensions of the CGS are a subset of those
of the SI the mapping of types from SI to CGS is unambiguous.

Also note that complex as the type signature may be producing it is a
mostly mechanical process.

With the above two functions we can define the function that converts
a unit from the SI. We omit the type signature since it is hairy
but can be readily inferred.

> fromSI x = x /~ unit_SI  x *~ unit_CGS x


= Conversion to SI =

We use the same strategy to convert from CGS to SI. However, when
converting from CGS to SI there may be several valid SI dimensionalities
for any given CGS dimensionality. We will handle this ambiguity by
requiring the user to specify the desired type (except when it is
inferable) of the resulting quantity.  For example:

] toSI (3.2 *~ centi meter) :: Length Double

In order to do this we must employ lexically scoped type variables
and provide the hairy type signature for the 'toSI' function.

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
> toSI x = x /~ unit_CGS (undefined :: Quantity (Dim l m t i Zero Zero Zero) a)
>            *~ unit_SI  (undefined :: Quantity (Dim l m t i Zero Zero Zero) a)

Again, the type signature is complex but deriving it is a mechanical
process.


= 'Show' instance =

We round off by writing 'Show' instance for 'CGSDim' analogous to
that of 'Dim'.

Out of laziness we use the notation "sqrt(cm)" to represent halves
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


= Examples =

Let us try the Coulomb attraction example from [2]. We start by
performing the calculation in the SI.

> q_si  = 1.6021773e-19 *~ SI.coulomb -- Elementary charge in SI.
> r_si  = 0.1 *~ SI.nano SI.meter     -- Distance in SI
> f_si  = q_si ^ pos2 / (_4 * pi * e0 * r_si ^ pos2) 
>   where 
>       e0 = 8.8541878e-12 *~ (SI.ampere * SI.second / (SI.volt * SI.meter)) 

The same calculation in the CGS system.

> q_cgs = fromSI q_si -- Elementary charge in CGS.
> r_cgs = fromSI r_si -- Distance in CGS
> f_cgs = q_cgs ^ pos2 / r_cgs ^ pos2

Inspecting the values in GHCi shows us that the results are consistent
(within reasonable accuracy) with [2].

  *Numeric.Dimensional.CGS> f_si
  2.3070794737101255e-8 m kg s^-2
  *Numeric.Dimensional.CGS> f_cgs 
  2.30708078598602e-3 sqrt(cm)^2 sqrt(g)^2 s^-2

To convert from CGS to SI we must specify the type of the SI 'Quantity'.

> f_si' = toSI f_cgs :: SIQ.Force Double

  *Numeric.Dimensional.CGS> f_si'
  2.3070807859860202e-8 m kg s^-2

We follow up with another conversion example demonstrating the
ambiguity in the conversion from CGS to SI.

> c     = 1 *~ SI.farad -- A SI capacitance.
> c_cgs = fromSI c      -- Capacitance has dimensionality L in CGS.
> c'    = toSI c_cgs :: SIQ.Capacitance Double
> c''   = toSI c_cgs :: Length Double

  *Numeric.Dimensional.CGS> c
  1.0 m^-2 kg^-1 s^4 A^2
  *Numeric.Dimensional.CGS> c_cgs
  8.98755691740885e11 sqrt(cm)^2
  *Numeric.Dimensional.CGS> c'
  1.0 m^-2 kg^-1 s^4 A^2
  *Numeric.Dimensional.CGS> c''
  8.98755691740885e9 m


= Future work =

This is a very rudimentary implementation. To make it more practical
a significant number of quantities and units, in particularly those
commonly used with the CGS, would need to be added. In the mean
time all units defined for the SI can be used with the CGS by
applying 'fromSI' to quantities defined from the SI units.

If anyone is willing to add quantities/units (or other enhancements)
I will happily to accept patches. Personally I do not expect to use
this module and therefore do not intend to invest much more time
in it. If the module has other users I might reconsider.

And of course, another direction of future work is to define
additional systems (e.g. natural, relativistic) using this module
as a template. I imagine this should be fairly straight forward.


= References =

[1] http://code.google.com/p/dimensional/wiki/ChuckBlake20070611
[2] http://www.tf.uni-kiel.de/matwis/amat/mw1_ge/kap_2/basics/b2_1_14.html
