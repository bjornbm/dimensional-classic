Numeric.Dimensional.NonSI
Bjorn Buckwalter, bjorn.buckwalter@gmail.com
License: BSD3


= Summary =

This module defines units that are not part of the SI, with the
exception of those defined in the 'SIUnits' module (units outside
of the SI accepted for use with the SI).

Any chapters, sections or tables referenced are from [1] unless
otherwise specified.

> {- |
>    Copyright  : Copyright (C) 2006-2008 Bjorn Buckwalter
>    License    : BSD3
>
>    Maintainer : bjorn.buckwalter@gmail.com
>    Stability  : Stable
>    Portability: GHC only?
>
> Please refer to the literate Haskell code for documentation of both API
> and implementation.
> -}

> module Numeric.Units.Dimensional.NonSI where

> import Numeric.Units.Dimensional.Prelude
> import qualified Prelude


= Neper, bel, shannon and the like =

The units of section 5.1.2 are purposefully (but not permanently)
omitted. In fact the logarithmic units (see section 8.7) are
problematic and it is not clear how to implement them. Perhaps with
a conversion function similar to for degrees Celsius.


= Table 7 =

"Units accepted for use with the SI whose values in SI units are
obtained experimentally."

When [1] was published The electronvolt had a standard combined
uncertainity of 0.00000049e-19 J and the unified atomic mass unit
had a combined uncertainty of 0.0000010e-27 kg.

> electronVolt :: Fractional a => Unit DEnergy a
> electronVolt = prefix 1.60217733e-19 joule
> unifiedAtomicMassUnit :: Fractional a => Unit DMass a
> unifiedAtomicMassUnit = prefix 1.6605402e-27 (kilo gram)


= Other units =

Some US customary (that is, inch-pound) units.

> inch, foot :: Fractional a => Unit DLength a
> inch = prefix 2.54 (centi meter)
> foot = prefix 12 inch     -- 0.3048 m
> poundMass, ounce :: Fractional a => Unit DMass a
> poundMass = prefix 0.45359237 (kilo gram)
> ounce     = prefix 28.349523 gram

In order to relate pounds mass to pounds force we define the
questionable unit 'gee' (G) as the gravitational acceleration at
sea level. Note that 'gee' is experimental and has an inherent
uncertainty which also transfers to 'poundForce'.

> gee :: Fractional a => Unit DAcceleration a
> gee = prefix 9.80665 meter / second ^ pos2
> poundForce :: Fractional a => Unit DForce a
> poundForce = poundMass * gee  -- 4.4482 N

Other (non inch-pound) units.

> yard, mile, nauticalMile :: (Fractional a) => Unit DLength a
> yard = prefix 3 foot
> mile = prefix 1760 yard
> nauticalMile = prefix 1852 meter
> revolution :: (Floating a) => Unit DOne a
> revolution = prefix 360 degree
> bar :: (Fractional a) => Unit DPressure a
> bar = prefix 1.0e5 pascal
> teaspoon :: (Fractional a) => Unit DVolume a
> teaspoon = prefix 5 (milli liter)


The IAU recommends[2] that:

  Although there are several different kinds of year (as there are
  several kinds of day), it is best to regard a year as a julian
  year of 365.25 days (31.5576 Ms) unless otherwise specified.

This aligns well with my needs so I'm happy to oblige. We define
the year in terms of seconds in order to avoid a 'Fractional'
constraint, and also provide a Julian century.

> year, century :: Num a => Unit DTime a
> year    = prefix 31557600 second
> century = prefix 100 year


= References =

[1] http://physics.nist.gov/Pubs/SP811/
[2] http://www.iau.org/science/publications/proceedings_rules/units/
