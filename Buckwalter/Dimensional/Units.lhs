
> module Buckwalter.Dimensional.Units where

> import Prelude (Num, Fractional, Floating)
> import qualified Prelude as P
> import Buckwalter.Dimensional
> import Buckwalter.NumType ( Neg2, Neg1, Zero, Pos1, Pos2, Pos3
>                           , neg2, neg1, zero, pos1, pos2, pos3
>                           )


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


= Powers of length units =

It is permissible to express powers of length units by prefixing
'square' and 'cubic' (9.6 "Spelling unit names raised to powers"
of [1]).

> square :: (Num a) => Unit DLength a -> Unit DArea a
> square x = x ^+ pos2
> cubic  :: (Num a) => Unit DLength a -> Unit DVolume a
> cubic  x = x ^+ pos3


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

Note that all the below time units are "nominal" units, in the sense
that they represent the given time span provided there was no leap
second or leap year.

> minute, hour, day, year :: Num a => Unit DTime a
> minute = prefix 60 second
> hour   = prefix 60 minute
> day    = prefix 24 hour
> year   = prefix 365 day

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
> revolution :: (Floating a) => Unit DOne a
> revolution = prefix 360 degree


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

