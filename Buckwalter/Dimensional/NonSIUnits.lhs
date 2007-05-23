> module Buckwalter.Dimensional.NonSIUnits where

> import Buckwalter.Dimensional.Prelude


= Units outside the SI (section 5) =

The units accepted for use with the SI (section 5.1.1) were defined
in the 'SIUnits' module.


== Neper, bel, shannon and the like ==

The units of section 5.1.2 are purposefully omitted. In fact the
logarithmic units (see section 8.7) are problematic and it is not
clear how to implement them. Perhaps with a conversion function
similar to for degrees Celsius.


== Table 7 ==

"Units accepted for use with the SI whose values in SI units are
obtained experimentally."

The electronvolt had a standard combined uncertainity of 0.00000049e-19 J
when [1] was published.

> electronVolt :: Fractional a => Unit DEnergy a
> electronVolt = prefix 1.60217733e-19 joule

The unified atomic mass unit had a combined uncertainty of 0.0000010e-27 kg.

> unifiedAtomicMassUnit :: Fractional a => Unit DMass a
> unifiedAtomicMassUnit = prefix 1.6605402e-27 (kilo gram)


== Table 9 ==

"Units temporarily accepted for use with the SI."

TODO: put in NonSIUnits module.


= Other units =
