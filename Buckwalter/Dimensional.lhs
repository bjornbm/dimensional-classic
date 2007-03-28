Buckwalter.Dimensional -- Statically checked physical dimensions
Bjorn Buckwalter, bjorn.buckwalter@gmail.com
License: BSD3


= Summary =

In this module we provide data types for performing arithmetic with
physical quantities and units. Information about the physical
dimensions of the quantities/units is embedded in their types and
the validity of operations is verified by the type checker at compile
time. The boxing and unboxing of numerical values as quantities is
done by multiplication and division of units, of which an incomplete
set is provided.

We limit ourselves to "Newtonian" physics. We do not attempt to
accommodate relativistic physics in which e.g. addition of length
and time would be valid.

As far as possible and/or practical the conventions and guidelines
of NIST's "Guide for the Use of the International System of Units
(SI)" [1] are followed. Occasionally we will reference specific
sections from the guide and deviations will be explained.


= Disclaimer =

Merely an engineer, the author doubtlessly uses a language and
notation that makes mathematicians and physicist cringe. He does
not mind constructive criticism (or darcs patches).

The sets of functions and units defined herein are incomplete and
reflect only the author's needs to date. Again, patches are welcome.

The author has elected to keep the module detached from the standard(?)
Haskell library hierarchy. In part because the module name space
layout seems to be an open issue and in part because he is unsure
where to fit it in.


= Preliminaries =

This module requires GHC 6.6 or later. We utilize multi-parameter
type classes, phantom types, functional dependencies and undecidable
instances (and possibly additional unidentified GHC extensions).
Clients of the module are generally not required to use these
extensions.

> {-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

> module Buckwalter.Dimensional 
>       -- TODO discriminate exports, in particular Variants and Dims.
>   where

> import Prelude hiding
>   ((*), (/), (+), (-), (^), sqrt, negate, pi, sin, cos, exp)
> import qualified Prelude as P 
>   ((*), (/), (+), (-), (^), sqrt, negate, pi, sin, cos, exp)
> import Buckwalter.NumType (NumType, PosType, NegType, NonZero,
>                            Zero, Pos, Neg, asIntegral,
>                            Sum, Prod, 
>                            Pos1, Pos2, pos2, Pos3, pos3)
> import qualified Buckwalter.NumType as N (Div)

We will reuse the operators and function names from the Prelude.
To prevent unpleasant surprises we give operators the same fixity
as the Prelude.

> infixr 8  ^
> infixl 7  *, /
> infixl 6  +, -


= Dimensional =

Our primary objective is to define a data type that can be used to
represent (while still differentiating between) units and quantities.
There are two reasons for consolidating units and quantities in one
data type. The first being to allow code reuse as they are largely
subject to the same operations. The second being that it allows
reuse of operators (and functions) between the two without resorting
to occasionally cumbersome type classes.

We call this data type 'Dimensional' to capture the notion that the
units and quantities it represents have physical dimensions.

> newtype Dimensional v d a = Dimensional a deriving (Show, Eq, Ord)

The type variable 'a' is the only non-phantom type variable and
represents the numerical value of a quantity or the scale (w.r.t.
SI units) of a unit. For SI units the scale will always be 1. For
non-SI units the scale is the ratio of the unit to the SI unit with
the same physical dimension.

Since 'a' is the only non-phantom type we were able to define
'Dimensional' as a newtype, avoiding boxing at runtime.


= The variety 'v' of 'Dimensional' =

The phantom type variable v is used to distinguish between units
and quantities. It should be one of the following:

> data DUnit
> data DQuantity

For convenience we define type synonyms for units and quantities.

> type Unit     = Dimensional DUnit
> type Quantity = Dimensional DQuantity

The relationship between (the value of) a 'Quantity', its numerical
value and its 'Unit' is described in 7.1 "Value and numerical value
of a quantity" of [1]. In short a 'Quantity' is the product of a
number and a 'Unit'. We define the '(*~)' operator as a convenient
way to declare quantities as such a product.

> (*~) :: Num a => a -> Unit d a -> Quantity d a
> x *~ Dimensional y = Dimensional (x P.* y)

Conversely, the numerical value of a 'Quantity' is obtained by
dividing the 'Quantity' by its 'Unit' (any unit with the same
physical dimension). The '(/~)' operator provides a convenient way
of obtaining the numerical value of a quantity.

> (/~) :: Fractional a => Quantity d a -> Unit d a -> a
> Dimensional x /~ Dimensional y = x P./ y

We give '*~' and '/~' the same fixity as '*' and '/' defined below.
Note that this necessitates the use of parenthesis when composing 
units using '*' and '/', e.g. "1 *~ (meter / second)".

> infixl 7  *~, /~


= The dimension 'd' of 'Dimensional' =

The phantom type variable d encompasses the physical dimension of
the 'Dimensional'. As detailed in [1] there are seven base dimensions,
which can be combined in integer powers to a given physical dimension.
We represent physical dimensions as the powers of the seven base
dimensions that make up the given dimension. The powers are represented
using NumTypes. For convenience we collect all seven base dimensions
in a data type 'Dim'.

> data (NumType l,    -- Length.
>       NumType m,    -- Mass.
>       NumType t,    -- Time.
>       NumType i,    -- Electric current.
>       NumType th,   -- Thermodynamic temperature.
>       NumType n,    -- Amount of substance.
>       NumType j)    -- Luminous intensity.
>   => Dim l m t i th n j 

We could have chosen to provide type variables for the seven base
dimensions in 'Dimensional' instead of creating a new data type
'Dim'. However, that would have made any type signatures involving
'Dimensional' very cumbersome.  By encompassing the physical dimension
in a single type variable we can "hide" the cumbersome type arithmetic
behind convenient type classes as will be seen later.

Using our 'Dim' data type we define some type synonyms for convenience
and illustrative purposes. We start with the base dimensions.

> type DOne         = Dim Zero Zero Zero Zero Zero Zero Zero
> type DLength      = Dim Pos1 Zero Zero Zero Zero Zero Zero
> type DMass        = Dim Zero Pos1 Zero Zero Zero Zero Zero
> type DTime        = Dim Zero Zero Pos1 Zero Zero Zero Zero
> type DCurrent     = Dim Zero Zero Zero Pos1 Zero Zero Zero
> type DTemperature = Dim Zero Zero Zero Zero Pos1 Zero Zero
> type DAmount      = Dim Zero Zero Zero Zero Zero Pos1 Zero
> type DIntensity   = Dim Zero Zero Zero Zero Zero Zero Pos1


Using the above type synonyms we can define type synonyms for
quantities of particular physical dimensions.

Quantities with the base dimensions.

> type Dimensionless = Quantity DOne
> type Length        = Quantity DLength
> type Mass          = Quantity DMass
> type Time          = Quantity DTime
> type Current       = Quantity DCurrent
> type Temperature   = Quantity DTemperature
> type Amount        = Quantity DAmount
> type Intensity     = Quantity DIntensity


= Arithmetic on physical dimensions =

When performing arithmetic on units and quantities the arithmetics
must be applied to both the numerical values of the Dimensionals
but also to their physical dimensions. The type level arithmetic
on physical dimensions is governed by multi-parameter type classes
and functional dependences.

Multiplication of dimensions corresponds to adding of the base
dimensions' exponents.

> class Mul d d' d'' | d d' -> d'', d d'' -> d', d' d'' -> d
> instance (Sum l  l'  l'',
>           Sum m  m'  m'',
>           Sum t  t'  t'',
>           Sum i  i'  i'',
>           Sum th th' th'',
>           Sum n  n'  n'',
>           Sum j  j'  j'') => Mul (Dim l   m   t   i   th   n   j)
>                                  (Dim l'  m'  t'  i'  th'  n'  j')
>                                  (Dim l'' m'' t'' i'' th'' n'' j'')

Division of dimensions corresponds to subtraction of the base
dimensions' exponents.

> class Div d d' d'' | d d' -> d'', d d'' -> d', d' d'' -> d
> instance (Sum l  l'  l'',
>           Sum m  m'  m'',
>           Sum t  t'  t'',
>           Sum i  i'  i'',
>           Sum th th' th'',
>           Sum n  n'  n'',
>           Sum j  j'  j'') => Div (Dim l'' m'' t'' i'' th'' n'' j'')
>                                  (Dim l'  m'  t'  i'  th'  n'  j')
>                                  (Dim l   m   t   i   th   n   j)


= Arithmetic on units and quantities =

Thanks to the arithmetic on physical dimensions having been sorted
out separately a lot of the arithmetic on Dimensionals is straight
forward. In particular the type signatures are much simplified.

Multiplication, division and powers apply to both units and quantities.

> (*) :: (Num a, Mul d d' d'') 
>     => Dimensional v d a -> Dimensional v d' a -> Dimensional v d'' a
> Dimensional x * Dimensional y = Dimensional (x P.* y)

> (/) :: (Fractional a, Div d d' d'') 
>     => Dimensional v d a -> Dimensional v d' a -> Dimensional v d'' a
> Dimensional x / Dimensional y = Dimensional (x P./ y)


= Powers =

We limit ourselves to integer powers of Dimensionals as fractional
powers make little physical sense. Since the value of the exponent
affects the type of the result the value of the exponent must be
visible to the type system, therefore we will generally represent
the exponent with a 'NumType'. 

The following class and single instance would suffice for any
NumType.  However, it constrains the type 'a' to be a member of the
'Fractional' class.

> class (NumType n) => Power d n d' | d n -> d' where 
>   (^) :: (Fractional a) => Dimensional v d a -> n -> Dimensional v d' a
>   Dimensional x ^ n = Dimensional $ x ^^ asIntegral n

> instance (Prod l  x l',
>           Prod m  x m',
>           Prod t  x t',
>           Prod i  x i',
>           Prod th x th',
>           Prod n  x n',
>           Prod j  x j') => Power (Dim l   m   t   i   th   n   j) x 
>                                  (Dim l'  m'  t'  i'  th'  n'  j')

In the unlikely case someone needs to use this library with
non-fractional numbers we provide an alternative restricted to
positive exponents.

> class (NumType n) => Power' d n d' | d n -> d' where 
>   (^+) :: (Num a) => Dimensional v d a -> n -> Dimensional v d' a
>   Dimensional x ^+ n = Dimensional $ x P.^ asIntegral n

> instance (Prod l  x l',
>           Prod m  x m',
>           Prod t  x t',
>           Prod i  x i',
>           Prod th x th',
>           Prod n  x n',
>           Prod j  x j') => Power' (Dim l   m   t   i   th   n   j) x 
>                                   (Dim l'  m'  t'  i'  th'  n'  j')

> {-

To avoid the unnecessary 'Fractional' constraint for zero or positive
exponents we need to add 'a' as a class parameter and provide three
instances; one for zero exponents, one for positive exponents, and
one for negative exponents. Indeed the 'Fractional' constraint only
applies to negative exponents.

> class (NumType n) => Power a d n d' | d n -> d' 
>   where (^) :: Dimensional v d a -> n -> Dimensional v d' a

Using a zero exponent trivially results in dimensionless 1.

> instance (Num a) => Power a d Zero DOne where _ ^ _ = Dimensional 1

Positive exponents.

> instance (Num a, 
>           Prod l  (Pos x) l',
>           Prod m  (Pos x) m',
>           Prod t  (Pos x) t',
>           Prod i  (Pos x) i',
>           Prod th (Pos x) th',
>           Prod n  (Pos x) n',
>           Prod j  (Pos x) j') 
>        => Power a       (Dim l   m   t   i   th   n   j)
>                 (Pos x) (Dim l'  m'  t'  i'  th'  n'  j')
>   where
>       (Dimensional x) ^ n = Dimensional $ x P.^ asIntegral n

Negative exponents. Note that in contrast to the standard Prelude
we can use '^' for negative exponents too (the standard Prelude
uses '^^').

> instance (Fractional a,
>           Prod l  (Neg x) l',
>           Prod m  (Neg x) m',
>           Prod t  (Neg x) t',
>           Prod i  (Neg x) i',
>           Prod th (Neg x) th',
>           Prod n  (Neg x) n',
>           Prod j  (Neg x) j') 
>        => Power a       (Dim l   m   t   i   th   n   j)
>                 (Neg x) (Dim l'  m'  t'  i'  th'  n'  j')
>   where
>       (Dimensional x) ^ n = Dimensional $ x ^^ asIntegral n

> -}

A special case is that dimensionless quantities are not restricted
to integer powers. This is accommodated by 'Dimensionless' providing
an instance of 'Floating' in the Dimensionless module.


= Roots =

Roots could conceivably be applied to units but valid reasons
for doing so elude the author, so their use will be limited to
quantities.

> class (NonZero n) => Root d n d' | d n -> d' where 
>   nroot :: (Floating a) => n -> Quantity d a -> Quantity d' a
>   nroot n (Dimensional x) = Dimensional $ x ** (1 P./ (fromIntegral . asIntegral) n)

> instance (N.Div l  x l',
>           N.Div m  x m',
>           N.Div t  x t',
>           N.Div i  x i',
>           N.Div th x th',
>           N.Div n  x n',
>           N.Div j  x j') => Root (Dim l   m   t   i   th   n   j) x 
>                                  (Dim l'  m'  t'  i'  th'  n'  j')

We provide short-hands for the square and cubic roots.

> sqrt :: (Floating a, Root d Pos2 d') => Quantity d a -> Quantity d' a
> sqrt = nroot pos2
> cbrt :: (Floating a, Root d Pos3 d') => Quantity d a -> Quantity d' a
> cbrt = nroot pos3

We also provide an operator alternative to nroot for those that
prefer such.

> (^/) :: (Floating a, Root d n d') => Quantity d a -> n -> Quantity d' a
> (^/) = flip nroot

TODO: investigate non-termination of "sqrt $ (3 *~ meter) ^ pos3"


= Quantity operations =

Some operations only make sense for quantities. Of these, negation,
addition and subtraction are particularly simple as they are done
in a single physical dimension.

> negate :: (Num a) => Quantity d a -> Quantity d a
> negate (Dimensional x) = Dimensional (P.negate x)

> (+) :: (Num a) => Quantity d a -> Quantity d a -> Quantity d a
> Dimensional x + Dimensional y = Dimensional (x P.+ y)

> (-) :: (Num a) => Quantity d a -> Quantity d a -> Quantity d a
> x - y = x + negate y


= Elementary functions (incomplete set) =

Sine and cosine make sense only for angles (the type synonym 'Angle'
is defined later).

The exponential function only makes sense for dimensionless quantities.

> exp :: (Floating a) => Dimensionless a -> Dimensionless a
> exp (Dimensional x) = Dimensional (P.exp x)

> instance Functor Dimensionless where
>   fmap f (Dimensional x) = Dimensional (f x)


= Unit prefixes =

Prefixes are used to form decimal multiples and submultiples of SI
Units as described in 4.4 of [1]. We will define the SI prefixes
in terms of a 'prefix' function which applies a scale factor to a
unit. (The 'prefix' function will also be used to define non-SI
units.)

> prefix :: (Num a) => a -> Unit d a -> Unit d a
> prefix x (Dimensional y) = Dimensional (x P.* y)

We define all SI prefixes, from Table 5 in [1]. Multiples first.

> deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta 
>   :: Num a => Unit d a -> Unit d a
> deca  = prefix 10
> hecto = deca . deca
> kilo  = deca . hecto
> mega  = kilo . kilo
> giga  = kilo . mega
> tera  = kilo . giga
> peta  = kilo . tera
> exa   = kilo . peta
> zetta = kilo . exa
> yotta = kilo . zetta

Then the submultiples.
 
> deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto
>   :: Fractional a => Unit d a -> Unit d a
> deci  = prefix 0.1
> centi = deci . deci
> milli = deci . centi
> micro = milli . milli
> nano  = milli . micro
> pico  = milli . nano
> femto = milli . pico
> atto  = milli . femto
> zepto = milli . atto
> yocto = milli . zepto

By defining SI prefixes as functions applied to a 'Unit' we satisfy
6.2.6 "Unacceptability of stand-alone prefixes" of [1].


= SI base and derived units (incomplete) =

Finally we define the units. To avoid a myriad of one-letter functions
that would doubtlessly cause clashes and frustration in users' code
we spell out all unit names in full, as we did for prefixes. We
also elect to spell the unit names in singular form, as allowed by
9.7 "Other spelling conventions" of [1].

The first unit we will define is 'one'. The unit one has dimension
one and is the base unit of dimensionless values. As detailed in
7.10 "Values of quantities expressed simply as numbers: the unit
one, symbol 1" of [1] the unit one generally does not appear in
expressions. However, for us it is necessary to use 'one' as we
would any other unit to perform the "boxing" of dimensionless values.

> one     :: Num a => Unit DOne a
> one     = Dimensional 1

We continue by defining the other SI base units (see 4.1 of [1]).

> meter   :: Num a => Unit DLength a
> meter   = Dimensional 1

For mass the SI base unit is kilogram. For sensible prefixes we
define gram here (see 6.2.7 "Prefixes and the kilogram" in [1]).
The drawback is that we are forced to use 'Fractional'.

> gram    :: Fractional a => Unit DMass a
> gram    = Dimensional 1e-3
> second  :: Num a => Unit DTime a
> second  = Dimensional 1
> ampere  :: Num a => Unit DCurrent a
> ampere  = Dimensional 1
> kelvin  :: Num a => Unit DTemperature a
> kelvin  = Dimensional 1
> mole    :: Num a => Unit DAmount a
> mole    = Dimensional 1
> candela :: Num a => Unit DIntensity a
> candela = Dimensional 1


= Conclusion and usage =

We have defined operators and units that allow us to define and
work with physical quantities. A physical quantity is defined by
multiplying a number with a unit (the type signature is optional).

] v :: Velocity Prelude.Double
] v = 90 *~ (kilo meter / hour)

It follows naturally that the numerical value of a quantity is
obtained by division by a unit.

] numval :: Prelude.Double
] numval = v /~ (meter / second)
 
The notion of a quantity as the product of a numerical value and a
unit is supported by 7.1 "Value and numerical value of a quantity"
of [1]. While the above syntax is fairly natural it is unfortunate
that it must violate a number of the guidelines in [1], in particular
9.3 "Spelling unit names with prefixes", 9.4 "Spelling unit names
obtained by multiplication", 9.5 "Spelling unit names obtained by
division".

As a more elaborate example of how to use the module we define a
function for calculating the escape velocity of a celestial body
[2].

] escapeVelocity :: (Floating a) => Mass a -> Length a -> Velocity a
] escapeVelocity m r = sqrt (two * g * m / r)
]   where 
]       two = 2 *~ one
]       g = 6.6720e-11 *~ (newton * meter ^ pos2 / kilo gram ^ pos2)

The following is an example GHC session where the above function
is used to calculate the escape velocity of Earth in kilometer per
second.

  *Buckwalter.Dimensional> :set +t
  *Buckwalter.Dimensional> let me = 5.9742e24 *~ kilo gram -- Mass of Earth.
  me :: Quantity DMass GHC.Float.Double
  *Buckwalter.Dimensional> let re = 6372.792 *~ kilo meter -- Mean radius of Earth.
  re :: Quantity DLength GHC.Float.Double
  *Buckwalter.Dimensional> let ve = escapeVelocity me re   -- Escape velocity of Earth.
  ve :: Velocity GHC.Float.Double
  *Buckwalter.Dimensional> ve /~ (kilo meter / second)
  11.184537332296259
  it :: GHC.Float.Double

For completeness we should also show an example of the error messages
we will get from GHC when performing invalid arithmetic. In the
best case GHC will be able to use the type synonyms we have defined
in its error messages.

] x = 1 *~ meter + 1 *~ second

    Couldn't match expected type `Pos1' against inferred type `Zero'
      Expected type: Unit DLength t
      Inferred type: Unit DTime a
    In the second argument of `(*~)', namely `second'
    In the second argument of `(+)', namely `1 *~ second'

In other cases the error messages aren't very friendly.

] x = 1 *~ meter / (1 *~ second) + 1 *~ kilo gram

    Couldn't match expected type `Zero'
           against inferred type `Neg Zero'
    When using functional dependencies to combine
      Sub Zero (Pos Zero) (Neg Zero),
        arising from use of `/' at Buckwalter/Dimensional.lhs:425:9-20
      Sub Zero (Pos Zero) Zero,
        arising from use of `/' at Buckwalter/Dimensional.lhs:532:5-30

It is the author's experience that the usefullness of the compiler
error messages is more often than not limited to pinpointing the
location of errors.


= Future work =

While there is an insane amount of units in use around the world
it is reasonable to provide at least all SI units. Units outside
of SI will most likely be added on an as-needed basis. 

There are also plenty of elementary functions to add. The 'Floating'
class can be used as reference.

Another useful addition would be decent 'Show' and 'Read' instances.
The 'show' implementation could output the numerical value and the
unit expressed in (base?) SI units, along the lines of:

] instance (Fractional a, Show a) => Show (Length a) 
]   where show x = show (x /~ meter) ++ " m"

Additional functions could be provided for "showing" with any unit
and prefix.  The 'read' implementation should be able to read values
with any unit and prefix. It is not clear to the author how to best 
implement these.

Additional physics models could be implemented. See [3] for ideas.


= Related work =

Henning Thielemann numeric prelude has a physical units library,
however, checking of dimensions is dynamic rather than static.
Aaron Denney has created a toy example of statically checked
physical dimensions covering only length and time. HaskellWiki
has pointers [4] to these.

Libraries with similar functionality exist for other programming
languages and may serve as inspiration. The author has found the
Java library JScience [5] and the Fortress programming language [6]
particularly noteworthy.


= References =

[1] http://physics.nist.gov/Pubs/SP811/
[2] http://en.wikipedia.org/wiki/Escape_velocity
[3] http://jscience.org/api/org/jscience/physics/models/package-summary.html
[4] http://www.haskell.org/haskellwiki/Physical_units
[5] http://jscience.org/
[6] http://research.sun.com/projects/plrg/fortress.pdf

