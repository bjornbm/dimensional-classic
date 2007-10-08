Numeric.Dimensional.NonSI
Bjorn Buckwalter, bjorn.buckwalter@gmail.com
License: BSD3


= Summary =

This module defines units that are not part of the SI, with the
exception of those defined in the 'SIUnits' module (units outside
of the SI accepted for use with the SI). 

Any chapters, sections or tables referenced are from [1] unless
otherwise specified.

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
> poundMass :: Fractional a => Unit DMass a
> poundMass = prefix 0.45359237 (kilo gram)

In order to relate pounds mass to pounds force we define the
questionable unit 'gee' (G) as the gravitational acceleration at
sea level. Note that 'gee' is experimental and has an inherent
uncertainty which also transfers to 'poundForce'.

> gee :: Fractional a => Unit DAcceleration a
> gee = prefix 9.80665 meter / second ^ pos2
> poundForce :: Fractional a => Unit DForce a
> poundForce = poundMass * gee  -- 4.4482 N

Other (non inch-pound) units.

> bar :: (Fractional a) => Unit DPressure a
> bar = prefix 1.0e5 pascal
> revolution :: (Floating a) => Unit DOne a
> revolution = prefix 360 degree


= References =

[1] http://physics.nist.gov/Pubs/SP811/

