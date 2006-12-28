Buckwalter.Dimensional -- Statically checked physical dimensions
Bjorn Buckwalter, bjorn@buckwalter.se
2006-12-19, version 0.1 (experimental)
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
> import Buckwalter.NumType (NumType, PosType, NegType, 
>                            Zero, Pos, Neg,
>                            incr, decr, Add, Sub, Halve, 
>                            zero,
>                            Pos1, Pos2, Pos3, pos1, pos2, pos3, 
>                            Neg1, Neg2, Neg3, neg1, neg2, neg3)

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

> newtype (Variant v, Dims d) 
>      => Dimensional v d a = Dimensional a deriving (Show, Eq, Ord)

The type variable 'a' is the only non-phantom type variable and
represents the numerical value of a quantity or the scale (w.r.t.
SI units) of a unit. For SI units the scale will always be 1. For
non-SI units the scale is the ratio of the unit to the SI unit with
the same physical dimension.

Since 'a' is the only non-phantom type we were able to define
'Dimensional' as a newtype, avoiding boxing at runtime.


= The variant 'v' of 'Dimensional' =

The phantom type variable v is used to distinguish between units
and quantities. It should be one of the following:

> data DUnit
> data DQuantity

We enforce this using the (non-exported) type class 'Variant' of
which 'DUnit' and 'DQuantity' are the only instances.

> class Variant v
> instance Variant DUnit
> instance Variant DQuantity

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
in a data type 'Dim' which is the sole instance of the 'Dims' type
class.

> data (NumType l,    -- Length.
>       NumType m,    -- Mass.
>       NumType t,    -- Time.
>       NumType i,    -- Electric current.
>       NumType th,   -- Thermodynamic temperature.
>       NumType n,    -- Amount of substance.
>       NumType j)    -- Luminous intensity.
>   => Dim l m t i th n j 

> class Dims d
> instance Dims (Dim l m t i th n j)

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

We add some derived physical dimensions.

> type DArea         = Dim Pos2 Zero Zero Zero Zero Zero Zero
> type DVolume       = Dim Pos3 Zero Zero Zero Zero Zero Zero
> type DFrequency    = Dim Zero Zero Neg1 Zero Zero Zero Zero
> type DVelocity     = Dim Pos1 Zero Neg1 Zero Zero Zero Zero
> type DAcceleration = Dim Pos1 Zero Neg2 Zero Zero Zero Zero
> type DForce        = Dim Pos1 Pos1 Neg2 Zero Zero Zero Zero
> type DImpulse      = Dim Pos1 Pos1 Neg1 Zero Zero Zero Zero
> type DMassFlow     = Dim Zero Pos1 Neg1 Zero Zero Zero Zero
> type DPressure     = Dim Neg1 Pos1 Neg2 Zero Zero Zero Zero

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

Some quantities with derived dimensions.

> type Frequency       = Quantity DFrequency
> type Velocity        = Quantity DVelocity
> type Angle           = Dimensionless
> type SolidAngle      = Dimensionless
> type AngularVelocity = Frequency
> type Force           = Quantity DForce
> type Thrust          = Force
> type Impulse         = Quantity DImpulse
> type MassFlow        = Quantity DMassFlow
> type Pressure        = Quantity DPressure


= Arithmetic on physical dimensions =

When performing arithmetic on units and quantities the arithmetics
must be applied to both the numerical values of the Dimensionals
but also to their physical dimensions. The type level arithmetic
on physical dimensions is governed by multi-parameter type classes
and functional dependences.

Multiplication of dimensions corresponds to adding of the base
dimensions' exponents.

> class Mul d d' d'' | d d' -> d'' 
> instance (Add l  l'  l'',
>           Add m  m'  m'',
>           Add t  t'  t'',
>           Add i  i'  i'',
>           Add th th' th'',
>           Add n  n'  n'',
>           Add j  j'  j'') => Mul (Dim l   m   t   i   th   n   j)
>                                  (Dim l'  m'  t'  i'  th'  n'  j')
>                                  (Dim l'' m'' t'' i'' th'' n'' j'')

Division of dimensions corresponds to subtraction of the base
dimensions' exponents.

> class Div d d' d'' | d d' -> d'' 
> instance (Sub l  l'  l'',
>           Sub m  m'  m'',
>           Sub t  t'  t'',
>           Sub i  i'  i'',
>           Sub th th' th'',
>           Sub n  n'  n'',
>           Sub j  j'  j'') => Div (Dim l   m   t   i   th   n   j)
>                                  (Dim l'  m'  t'  i'  th'  n'  j')
>                                  (Dim l'' m'' t'' i'' th'' n'' j'')

Taking the square root of a dimension corresponds to halving the
base dimensions' exponents.

> class Sqrt d d' | d -> d'
> instance (Halve l  l',
>           Halve m  m',
>           Halve t  t',
>           Halve i  i',
>           Halve th th',
>           Halve n  n',
>           Halve j  j') => Sqrt (Dim l  m  t  i  th  n  j)
>                                (Dim l' m' t' i' th' n' j')


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

We limit ourselves to integer powers of Dimensionals as fractional
powers make little physical sense. Since the value of the exponent
affects the type of the result the value of the exponent must be
visible to the type system, therefore we will generally represent
the exponent with a 'NumType'. We must also use a type class to
capture the different behaviors of positive, zero and negative
exponents.

> class Power a d n d' | d n -> d' 
>   where (^) :: Dimensional v d a -> n -> Dimensional v d' a

Anything to the power of zero equals one.

> instance (Num a) => Power a d Zero DOne where _ ^ _ = Dimensional 1

Positive exponents.

> instance (Num a, PosType n, Power a d n d', Mul d' d d'') 
>       => Power a d (Pos n) d'' where x ^ n = x ^ decr n * x

Negative exponents.

> instance (Fractional a, NegType n, Power a d n d', Div d' d d'') 
>       => Power a d (Neg n) d'' where x ^ n = x ^ incr n / x

A special case is that dimensionless quantities are not restricted
to integer powers. Here we utilize the 'a' type variable of 'Power'
as well as the fact that the 'n' type variable is not limited to
NumTypes to allow a dimensionless quantity to be raised to the power
of any other dimensionless quantity.

> instance (Floating a) => Power a DOne (Dimensionless a) DOne
>   where (Dimensional x) ^ (Dimensional y) = Dimensional (x ** y)

It is permissible to express powers of length units by prefixing
'square' and 'cubic' (9.6 "Spelling unit names raised to powers"
of [1]).

> square :: (Num a) => Unit DLength a -> Unit DArea a
> square x = x * x
> cubic :: (Num a) => Unit DLength a -> Unit DVolume a
> cubic  x = square x * x

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

It makes little sense to apply elementary functions to units so we
limit their use to quantities, and in many cases to quantities with
specific physical dimensions.

Square root could conceivably be applied to units but valid reasons
for doing so elude the author, so their use will be limited to
quantities.

> sqrt :: (Floating a, Sqrt d d') => Quantity d a -> Quantity d' a
> sqrt (Dimensional x) = Dimensional (P.sqrt x)

Sine and cosine make sense only for angles (the type synonym 'Angle'
is defined later).

> sin, cos :: (Floating a) => Angle a -> Dimensionless a
> sin (Dimensional x) = Dimensional (P.sin x)
> cos (Dimensional x) = Dimensional (P.cos x)

The exponential function only makes sense for dimensionless quantities.

> exp :: (Floating a) => Dimensionless a -> Dimensionless a
> exp (Dimensional x) = Dimensional (P.exp x)


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

Now we go on to define (a limited set of) the SI derived units in
terms of the base units (see 4.2 "SI derived units" of [1]).

> radian, steradian :: Fractional a => Unit DOne a
> radian    = meter / meter
> steradian = meter ^ pos2 / meter ^ pos2
> hertz :: Fractional a => Unit DFrequency a
> hertz = one / second
> newton :: Fractional a => Unit DForce a
> newton = kilo gram * meter / second ^ pos2
> pascal :: Fractional a => Unit DPressure a
> pascal = newton / meter ^ pos2
 

= Non-SI units =

We will define a small subset of the myriad of units that exists
outside of the SI. These are generally defined in terms of another
unit with a prefix (using the 'prefix' function defined earlier).

We start with (a limited set of) the units accepted for use with
the SI (see 5.1 of [1]).

> degree :: Floating a => Unit DOne a
> degree = prefix (P.pi P./ 180) radian
> minute, hour, day :: Num a => Unit DTime a
> minute = prefix 60 second
> hour   = prefix 60 minute
> day    = prefix 24 hour

We continue with units not accepted for use in with the SI, in
particular the US customary (that is, inch-pound) units.

> inch, foot :: Fractional a => Unit DLength a
> inch = prefix 2.54 (centi meter)
> foot = prefix 12 inch     -- 0.3048 m
> poundMass :: Fractional a => Unit DMass a
> poundMass = prefix 0.45359237 (kilo gram)

In order to relate pounds mass to pounds force we define the
questionable unit 'gee' (G) as the gravitational acceleration at
sea level.

> gee :: Fractional a => Unit DAcceleration a
> gee = prefix 9.80665 meter / second ^ pos2
> poundForce :: Fractional a => Unit DForce a
> poundForce = poundMass * gee  -- 4.4482 N

Other (non inch-pound) units.

> bar :: (Fractional a) => Unit DPressure a
> bar = prefix 1.0e5 pascal


= Less conformant units =

The unit system we have devised and used above will not work well
with some non-SI units. In particular units which do not scale
linearly with respect to the SI units, for example logarithmic units
(see 8.7 "Logarithmic quantities and units: level, neper, bel" of
[1]).

Another problematic area is units which increase proportionally to
the SI units but cross zero at a different point. An example would
be degrees Celsius. The author feels that it is appropriate to
define a unit for use with relative quantities (taking only into
account the proportionality) and complement the unit with functions
for converting absolute values.

> degreeCelsius :: Num a => Unit DTemperature a
> degreeCelsius = kelvin
> fromDegreeCelsiusAbsolute :: Fractional a => a -> Temperature a
> fromDegreeCelsiusAbsolute x = x *~ degreeCelsius + 273.15 *~ degreeCelsius
> toDegreeCelsiusAbsolute :: Fractional a => Temperature a -> a
> toDegreeCelsiusAbsolute x = (x - 273.15 *~ degreeCelsius) /~ degreeCelsius


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

