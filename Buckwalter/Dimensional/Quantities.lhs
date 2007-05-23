Buckwalter.Dimensional.Quantities
Bjorn Buckwalter, bjorn.buckwalter@gmail.com
License: BSD3


= Summary =

This module defines typ synonyms for common dimensionalities and
the associated quantity types. Additional dimensionalities and
quantity types will be added on an as-needed basis.

> module Buckwalter.Dimensional.Quantities where

> import Buckwalter.Dimensional
> import Buckwalter.NumType 
>   ( Neg3, Neg2, Neg1, Zero, Pos1, Pos2, Pos3, Pos4
>   )


== Table 2 ==

"Examples of SI derived units expressed in terms of SI base units."

The following definitions are grouped so that a type synonym for
the dimensionality is defined first in terms of base dimension
exponents.  Then a type synonym for the corresponding quantity type
is defined. If there are several alternate names for a quantity
type one type synonyms is provided for each name.

> type DArea = Dim Pos2 Zero Zero Zero Zero Zero Zero
> type Area  = Quantity DArea

> type DVolume = Dim Pos3 Zero Zero Zero Zero Zero Zero
> type Volume  = Quantity DVolume

> type DVelocity = Dim Pos1 Zero Neg1 Zero Zero Zero Zero
> type Velocity  = Quantity DVelocity

> type DAcceleration = Dim Pos1 Zero Neg2 Zero Zero Zero Zero
> type Acceleration  = Quantity DAcceleration

> type DWaveNumber = Dim Neg1 Zero Zero Zero Zero Zero Zero
> type WaveNumber  = Quantity DWaveNumber

> type DMassDensity = Dim Neg3 Pos1 Zero Zero Zero Zero Zero
> type MassDensity  = Quantity DMassDensity
> type Density      = MassDensity -- Short name.

> type DSpecificVolume = Dim Pos3 Neg1 Zero Zero Zero Zero Zero 
> type SpecificVolume  = Quantity DSpecificVolume

> type DCurrentDensity = Dim Neg2 Zero Zero Pos1 Zero Zero Zero
> type CurrentDensity  = Quantity DCurrentDensity

> type DMagneticFieldStrength = Dim Neg1 Zero Zero Pos1 Zero Zero Zero
> type MagneticFieldStrength  = Quantity DMagneticFieldStrength

> type DAmountOfSubstanceConcentration = Dim Neg3 Zero Zero Zero Zero Pos1 Zero
> type AmountOfSubstanceConcentration  = Quantity DAmountOfSubstanceConcentration
> type Concentration                   = AmountOfSubstanceConcentration -- Short name.

> type DLuminance = Dim Neg2 Zero Zero Zero Zero Zero Pos1
> type Luminance  = Quantity DLuminance


== Table 3a ==

"SI derived units with special names and symbols, including the
radian and steradian."

> type DPlaneAngle = DOne
> type PlaneAngle  = Dimensionless

> type DSolidAngle = DOne
> type SolidAngle  = Dimensionless

> type DFrequency = Dim Zero Zero Neg1 Zero Zero Zero Zero
> type Frequency  = Quantity DFrequency

> type DForce = Dim Pos1 Pos1 Neg2 Zero Zero Zero Zero
> type Force  = Quantity DForce

> type DPressure = Dim Neg1 Pos1 Neg2 Zero Zero Zero Zero
> type DStress   = DPressure
> type Pressure  = Quantity DPressure
> type Stress    = Quantity DStress

> type DEnergy         = Dim Pos2 Pos1 Neg2 Zero Zero Zero Zero
> type DWork           = DEnergy
> type DQuantityOfHeat = DEnergy
> type Energy          = Quantity DEnergy
> type Work            = Quantity DWork
> type QuantityOfHeat  = Quantity DQuantityOfHeat

> type DPower       = Dim Pos2 Pos1 Neg3 Zero Zero Zero Zero
> type DRadiantFlux = DPower
> type Power        = Quantity DPower
> type RadiantFlux  = Quantity DRadiantFlux


== Table 4 ==

"Examples of SI derived units expressed with the aid of SI derived
units having special names and symbols."

We use the same grouping as for table 2.

> type AngularVelocity = Frequency

> type DAngularAcceleration = Dim Zero Zero Neg2 Zero Zero Zero Zero
> type AngularAcceleration  = Quantity DAngularAcceleration

> type DDynamicViscosity = Dim Neg1 Pos1 Neg1 Zero Zero Zero Zero
> type DynamicViscosity  = Quantity DDynamicViscosity

> type MomentOfForce  = Quantity DEnergy

> type DSurfaceTension = Dim Zero Pos1 Neg2 Zero Zero Zero Zero
> type SurfaceTension  = Quantity DSurfaceTension

> type DHeatFluxDensity = Dim Zero Pos1 Neg3 Zero Zero Zero Zero
> type HeatFluxDensity  = Quantity DHeatFluxDensity
> type Irradiance       = HeatFluxDensity

> type RadiantIntensity = Power

> type Radiance = Irradiance

> type DHeatCapacity = Dim Pos2 Pos1 Neg2 Zero Neg1 Zero Zero
> type HeatCapacity  = Quantity DHeatCapacity
> type Entropy       = HeatCapacity

> type DSpecificHeatCapacity = Dim Pos2 Zero Neg2 Zero Neg1 Zero Zero
> type SpecificHeatCapacity  = Quantity DSpecificHeatCapacity
> type SpecificEntropy       = SpecificHeatCapacity

Specific energy was already defined in table 3b.

> type DThermalConductivity = Dim Pos1 Pos1 Neg3 Zero Neg1 Zero Zero
> type ThermalConductivity  = Quantity DThermalConductivity

> type EnergyDensity = Pressure

> type DElectricFieldStrength = Dim Pos1 Pos1 Neg3 Neg1 Zero Zero Zero
> type ElectricFieldStrength  = Quantity DElectricFieldStrength

> type DElectricChargeDensity = Dim Neg3 Zero Pos1 Pos1 Zero Zero Zero
> type ElectricChargeDensity  = Quantity DElectricChargeDensity

> type DElectricFluxDensity = Dim Neg2 Zero Pos1 Pos1 Zero Zero Zero
> type ElectricFluxDensity  = Quantity DElectricFluxDensity

> type DPermittivity = Dim Neg3 Neg1 Pos4 Pos2 Zero Zero Zero
> type Permittivity  = Quantity DPermittivity

> type DPermeability = Dim Pos1 Pos1 Neg2 Neg2 Zero Zero Zero
> type Permeability  = Quantity DPermeability

> type DMolarEnergy = Dim Pos2 Pos1 Neg2 Zero Zero Neg1 Zero
> type MolarEnergy  = Quantity DMolarEnergy

> type DMolarEntropy     = Dim Pos2 Pos1 Neg2 Zero Neg1 Neg1 Zero
> type MolarEntropy      = Quantity DMolarEntropy
> type MolarHeatCapacity = MolarEntropy

> type DExposure = Dim Zero Neg1 Pos1 Pos1 Zero Zero Zero
> type Exposure  = Quantity DExposure -- Exposure to x and gamma rays.

> type DAbsorbedDoseRate = Dim Pos2 Zero Neg3 Zero Zero Zero Zero
> type AbsorbedDoseRate  = Quantity DAbsorbedDoseRate



= Other quantities =

Some other quantity types that have come in handy.

> {-
> type Angle           = PlaneAngle -- Abbreviation
> type Thrust          = Force
> type Impulse         = Quantity DImpulse
> type MassFlow        = Quantity DMassFlow
> type EnergyPerUnitMass = SpecificEnergy


> type DImpulse      = Dim Pos1 Pos1 Neg1 Zero Zero Zero Zero
> type DMassFlow     = Dim Zero Pos1 Neg1 Zero Zero Zero Zero
> type DEnergyPerUnitMass = Dim Pos2 Zero Neg2 Zero Zero Zero Zero
> -}


= References =

[1] http://physics.nist.gov/Pubs/SP811/

