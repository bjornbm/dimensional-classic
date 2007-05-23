Buckwalter.Dimensional.SIUnits
Bjorn Buckwalter, bjorn.buckwalter@gmail.com
License: BSD3


= Summary =

This module defines the SI prefixes, the SI base units and the SI
derived units. It also defines the units outside of the SI that are
accepted for use with the SI.  Any chapters, sections or tables
referenced are from [1] unless otherwise specified.

> module Buckwalter.Dimensional.SIUnits where

> import Buckwalter.Dimensional
> import Buckwalter.Dimensional.Quantities
> import Buckwalter.NumType 
>   ( Neg3, Neg2, Neg1, Zero, Pos1, Pos2, Pos3, Pos4
>   , neg3, neg2, neg1, pos1, pos2, pos3
>   )
> import Prelude ( (.), Num, Fractional, Floating, recip )
> import qualified Prelude


= SI prefixes (section 4.4) =

Prefixes are used to form decimal multiples and submultiples of SI
Units as described in section 4.4. We will define the SI prefixes
in terms of the 'prefix' function which applies a scale factor to a
unit.

We define all SI prefixes from Table 5. Multiples first.

> deka, deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta 
>   :: Num a => Unit d a -> Unit d a
> deka  = prefix 10 -- International English.
> deca  = deka      -- American English.
> hecto = deka . deka
> kilo  = deka . hecto
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
section 6.2.6 "Unacceptability of stand-alone prefixes".


= SI base units (section 4.1) =

Now we will define the SI base unitsi from section 4.1. To avoid a
myriad of one-letter functions that would doubtlessly cause clashes
and frustration in users' code we spell out all unit names in full,
as we did for prefixes. We also elect to spell the unit names in
singular form, as allowed by section 9.7 "Other spelling conventions".

We define the SI base units in the order of table 1.

> metre, meter :: Num a => Unit DLength a
> metre = Dimensional 1 -- International English.
> meter = metre         -- American English.

For mass the SI base unit is kilogram. For sensible prefixes we
define gram here (see section 6.2.7 "Prefixes and the kilogram").
The drawback is that we are forced to use 'Fractional'.

> gram    :: Fractional a => Unit DMass a
> gram    = Dimensional 1e-3
> second  :: Num a => Unit DTime a
> second  = Dimensional 1
> ampere  :: Num a => Unit DElectricCurrent a
> ampere  = Dimensional 1
> kelvin  :: Num a => Unit DThermodynamicTemperature a
> kelvin  = Dimensional 1
> mole    :: Num a => Unit DAmountOfSubstance a
> mole    = Dimensional 1
> candela :: Num a => Unit DLuminousIntensity a
> candela = Dimensional 1


= SI derived units (section 4.2) =

Before defining the derived units themselves we provide type synonyms
for derived quantities and their dimensionalities. For lack of better
organization we provide definitions grouped by table in [1].


== Table 3a ==

"SI derived units with special names and symbols, including the
radian and steradian."

The following definitions are grouped so that a type synonym for
the dimensionality is defined first in terms of base dimension
exponents.  Then a type synonym for the corresponding quantity type
is defined. If there are several quantity types with the same
dimensionality a type synonym is provided for each quantity type.
Finally the unit is defined.

> radian :: Fractional a => Unit DPlaneAngle a
> radian = one -- meter * meter ^ neg1

> steradian :: Fractional a => Unit DSolidAngle a
> steradian = one -- meter ^ pos2 * meter ^ neg2

> hertz :: Fractional a => Unit DFrequency a
> hertz = second ^ neg1

> newton :: Fractional a => Unit DForce a
> newton = kilo gram * meter * second ^ neg2

> pascal :: Fractional a => Unit DPressure a
> pascal = newton / meter ^ pos2

> joule :: Fractional a => Unit DEnergy a
> joule = newton * meter

> watt :: Fractional a => Unit DPower a
> watt = joule / second

> type DElectricCharge       = Dim Zero Zero Pos1 Pos1 Zero Zero Zero
> type ElectricCharge        = Quantity DElectricCharge
> type QuantityOfElectricity = Quantity DElectricCharge
> coulomb :: Fractional a => Unit DElectricCharge a
> coulomb = second * ampere

> type DElectricPotential  = Dim Pos2 Pos1 Neg3 Neg1 Zero Zero Zero
> type ElectricPotential   = Quantity DElectricPotential
> type PotentialDifference = Quantity DElectricPotential
> type ElectromotiveForce  = Quantity DElectricPotential
> volt :: Fractional a => Unit DElectricPotential a
> volt = watt / ampere

> type DCapacitance = Dim Neg2 Neg1 Pos4 Pos2 Zero Zero Zero
> type Capacitance  = Quantity DCapacitance
> farad :: Fractional a => Unit DCapacitance a
> farad = coulomb / volt

> type DElectricResistance = Dim Pos2 Pos1 Neg3 Neg2 Zero Zero Zero
> type ElectricResistance  = Quantity DElectricResistance
> ohm :: Fractional a => Unit DElectricResistance a
> ohm = volt / ampere

> type DElectricConductance = Dim Neg2 Neg1 Pos3 Pos2 Zero Zero Zero
> type ElectricConductance  = Quantity DElectricConductance
> siemens :: Fractional a => Unit DElectricConductance a
> siemens = ampere / volt

> type DMagneticFlux = Dim Pos2 Pos1 Neg2 Neg1 Zero Zero Zero
> type MagneticFlux  = Quantity DMagneticFlux
> weber :: Fractional a => Unit DMagneticFlux a
> weber = volt * second

> type DMagneticFluxDensity = Dim Zero Pos1 Neg2 Neg1 Zero Zero Zero
> type MagneticFluxDensity  = Quantity DMagneticFluxDensity
> tesla :: Fractional a => Unit DMagneticFluxDensity a
> tesla = weber / meter ^ pos2

> type DInductance = Dim Pos2 Pos1 Neg2 Neg2 Zero Zero Zero
> type Inductance  = Quantity DInductance
> henry :: Fractional a => Unit DInductance a
> henry = weber / ampere

We defer the definition of Celcius temperature to the end (would
appear here if we stricly followed table 3a).

> type DLuminousFlux = DLuminousIntensity
> type LuminousFlux  = LuminousIntensity
> lumen :: Fractional a => Unit DLuminousFlux a
> lumen = candela / steradian

> type DIlluminance = Dim Neg2 Zero Zero Zero Zero Zero Pos1
> type Illuminance  = Quantity DIlluminance
> lux :: Fractional a => Unit DIlluminance a
> lux = lumen / meter ^ pos2

=== Degree Celsius ===

A problematic area is units which increase proportionally to the
base SI units but cross zero at a different point. An example would
be degrees Celsius (see section 4.2.1.1). The author feels that it
is appropriate to define a unit for use with relative quantities
(taking only into account the proportionality) and complement the
unit with functions for converting absolute values.

> type DCelsiusTemperature = DThermodynamicTemperature
> type CelsiusTemperature  = ThermodynamicTemperature
> degreeCelsius :: Num a => Unit DCelsiusTemperature a
> degreeCelsius = kelvin

The function 'fromDegreeCelsiusAbsolute' should be used in lieu of
"*~ degreeCelsius" when working with absolute temperatures. Similarily,
'toDegreeCelsiusAbsolute' should be used in lieu of "/~ degreeCelsius"
when working with absolute temperatures.

> fromDegreeCelsiusAbsolute :: Fractional a => a -> ThermodynamicTemperature a
> fromDegreeCelsiusAbsolute x = x *~ degreeCelsius + 273.15 *~ degreeCelsius
> toDegreeCelsiusAbsolute :: Fractional a => ThermodynamicTemperature a -> a
> toDegreeCelsiusAbsolute x = (x - 273.15 *~ degreeCelsius) /~ degreeCelsius


== Table 3b ==

"SI derived units with special names and symbols admitted for reasons
of safeguarding human health"

We use the same grouping as for table 3a.

> type DActivity = DFrequency -- Activity of a radionuclide.
> type Activity  = Quantity DActivity
> becquerel :: Fractional a => Unit DActivity a
> becquerel = second ^ neg1

Above we gave a new name to the dimensionality instead of reusing
'Frequency' in the quantity type definition. This will allow GHCi
be more specific when queried for the type of 'becquerel'. For
quantity types without a specific unit we don't bother doing this
(though perhaps we should in case there is a non-SI unit for the
quantity type?).

> type DAbsorbedDose  = Dim Pos2 Zero Neg2 Zero Zero Zero Zero
> type AbsorbedDose   = Quantity DAbsorbedDose
> type SpecificEnergy = Quantity DAbsorbedDose -- Specific energy imparted.
> type Kerma          = Quantity DAbsorbedDose
> gray :: Fractional a => Unit DAbsorbedDose a
> gray = joule / kilo gram

> type DDoseEquivalent           = DAbsorbedDose
> type DoseEquivalent            = Quantity DDoseEquivalent
> type AmbientDoseEquivalent     = DoseEquivalent
> type DirectionalDoseEquivalent = DoseEquivalent
> type PersonalDoseEquivalent    = DoseEquivalent
> type EquivalentDose            = DoseEquivalent
> sievert :: Fractional a => Unit DAbsorbedDose a
> sievert = joule / kilo gram


= Units outside the SI =

There are several units that are not strictly part of the SI but
are either permanently or temporarily accepted for use with the SI.
We define the permanently accepted ones in this module.

== Table 6 ==

"Units accepted for use with the SI."

We start with time which we grant exclusive rights to 'minute' and
'second'.

> minute, hour, day :: Num a => Unit DTime a
> minute = prefix 60 second
> hour   = prefix 60 minute
> day    = prefix 24 hour

Since 'minute' and 'second' are already in use for time we use
'arcminute' and 'arcsecond' [2] for plane angle instead.

> degree, arcminute, arcsecond :: Floating a => Unit DPlaneAngle a
> degree = prefix (Prelude.pi Prelude./ 180) radian
> arcminute = prefix (recip 60) degreeOfArc
> arcsecond = prefix (recip 60) minuteOfArc

Alternate (longer) forms of the above. In particular 'degreeOfArc'
can be used if there is a percieved need to disambiguate from e.g.
temperature.

> degreeOfArc, minuteOfArc, secondOfArc :: Floating a => Unit DPlaneAngle a
> degreeOfArc = degree
> secondOfArc = arcsecond
> minuteOfArc = arcminute

> litre, liter :: Fractional a => Unit DVolume a
> litre = deci meter ^ pos3 -- International English.
> liter = litre             -- American English.

> tonne, metricTon :: Fractional a => Unit DMass a
> tonne     = prefix 1000 (kilo gram) -- Name in original SI text.
> metricTon = tonne                   -- American name.


= References =

[1] http://physics.nist.gov/Pubs/SP811/
[2] http://en.wikipedia.org/wiki/Minute_of_arc
