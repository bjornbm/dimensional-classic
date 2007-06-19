
--    *** EXPERIMENTAL ***

{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module CGS where

import Prelude ( undefined, Num, Fractional, Floating, Show )
import qualified Prelude
import Buckwalter.Dimensional hiding ( DLength, DMass, DTime, DElectricCurrent )
import Buckwalter.Dimensional.Quantities
import qualified Buckwalter.Dimensional.SIUnits as SI
import qualified Buckwalter.NumType as N
import Buckwalter.NumType ( Neg2, Neg1, Zero, Pos, Pos1, Pos2, Pos3, NumType )
import Buckwalter.NumType ( neg2, neg1, zero, pos1, pos2, pos3 )
import Data.Maybe (catMaybes)


data CGSDim lh mh t
instance forall lh mh t.
    ( NumType lh
    , NumType mh
    , NumType t
    ) => Show (CGSDim lh mh t) where
    show _ = (Prelude.unwords Prelude.. catMaybes)
             [ dimUnit "sqrt(cm)" (undefined :: lh)
             , dimUnit "sqrt(g)"  (undefined :: mh)
             , dimUnit "s"  (undefined :: t)
             ]



-- instance Show (CGSDim lh mh t) where show _ = "CGS"

instance ( N.Sum lh lh' lh''
         , N.Sum mh mh' mh''
         , N.Sum t  t'  t'' ) => Mul (CGSDim lh   mh   t) 
                                     (CGSDim lh'  mh'  t') 
                                     (CGSDim lh'' mh'' t'')

instance ( N.Sum lh lh' lh''
         , N.Sum mh mh' mh''
         , N.Sum t  t'  t'' ) => Div (CGSDim lh'' mh'' t'') 
                                     (CGSDim lh'  mh'  t') 
                                     (CGSDim lh   mh   t)

instance ( N.Mul lh x lh'
         , N.Mul mh x mh'
         , N.Mul t  x t' ) => Pow (CGSDim lh  mh  t) x 
                                  (CGSDim lh' mh' t')

instance ( N.Div lh x lh'
         , N.Div mh x mh'
         , N.Div t  x t' ) => Root (CGSDim lh  mh  t) x 
                                   (CGSDim lh' mh' t')

-- Base dimensions.
type DLength = CGSDim Pos2 Zero Zero
type DMass   = CGSDim Zero Pos2 Zero
type DTime   = CGSDim Zero Zero Pos1

meter  :: Num a => Unit DLength a
meter  = Dimensional 100
gram   :: Num a => Unit DMass a
gram   = Dimensional 1
second :: Num a => Unit DTime a
second = Dimensional 1

-- Current
type DElectricCurrent = CGSDim Pos3 Pos1 Neg2
ampere :: Floating a => Unit DElectricCurrent a
ampere = prefix 3.33564e10 ((SI.centi meter ^ pos3) ^/ pos2 * gram ^/ pos2 * second ^ neg2)

-- Charge
type DCharge = CGSDim Pos3 Pos1 Neg1
franklin :: Floating a => Unit DCharge a
franklin = gram ^/ pos2 * (SI.centi meter ^ pos3) ^/ pos2 / second


-- Get the SI base unit of a Quantity.
unit_SI :: forall a l m t i th n j.
        ( Fractional a
        , N.Mul Zero l  Zero, N.Mul Pos1 l  l
        , N.Mul Zero m  Zero, N.Mul Pos1 m  m
        , N.Mul Zero t  Zero, N.Mul Pos1 t  t
        , N.Mul Zero i  Zero, N.Mul Pos1 i  i
        , N.Mul Zero th Zero, N.Mul Pos1 th th
        , N.Mul Zero n  Zero, N.Mul Pos1 n  n
        , N.Mul Zero j  Zero, N.Mul Pos1 j  j
        , N.Sum l  Zero l
        , N.Sum Zero m  m,  N.Sum m  Zero m
        , N.Sum Zero t  t,  N.Sum t  Zero t
        , N.Sum Zero i  i,  N.Sum i  Zero i
        , N.Sum Zero th th, N.Sum th Zero th
        , N.Sum Zero n  n,  N.Sum n  Zero n
        , N.Sum Zero j  j
        ) => Quantity (Dim l m t i th n j) a -> Unit (Dim l m t i th n j) a
unit_SI _ = SI.meter        ^ (undefined :: l)
          * SI.kilo SI.gram ^ (undefined :: m)
          * SI.second       ^ (undefined :: t)
          * SI.ampere       ^ (undefined :: i)
          * SI.kelvin       ^ (undefined :: th)
          * SI.mole         ^ (undefined :: n)
          * SI.candela      ^ (undefined :: j)

-- Alternate definition (this won't work for other than SI).
unit_SI' :: Num a => Quantity (Dim l m t i th n j) a -> Unit (Dim l m t i th n j) a
unit_SI' _ = Dimensional 1

-- Get the CGS unit corresponding to the SI base unit of a Quantity.
unit_CGS :: forall a l m t i l2 m2 il it l' m' t'.
         ( Floating a
         , N.Mul Zero l Zero, N.Mul Pos2 l l2
         , N.Mul Zero m Zero, N.Mul Pos2 m m2
         , N.Mul Zero t Zero, N.Mul Pos1 t t
         , N.Sum l2 Zero l2
         , N.Sum Zero m2 m2,  N.Sum m2 Zero m2
         , N.Sum Zero t  t
         , N.Mul Pos3 i  il
         , N.Mul Pos1 i  i
         , N.Mul Neg2 i  it
         , N.Sum l2 il l'
         , N.Sum m2 i  m'
         , N.Sum t  it t'
         ) => Quantity (Dim l m t i Zero Zero Zero) a -> Unit (CGSDim l' m' t') a
unit_CGS _ = meter        ^ (undefined :: l)
           * SI.kilo gram ^ (undefined :: m)
           * second       ^ (undefined :: t)
           * ampere       ^ (undefined :: i)

-- {-
fromSI x = x /~ unit_SI'  x *~ unit_CGS x
-- toSI   x = x /~ unit_CGS x *~ unit_SI  x

toSI :: forall a l m t i l2 m2 il it l' m' t'.
         ( Floating a
         , N.Mul Zero l Zero, N.Mul Pos2 l l2
         , N.Mul Zero m Zero, N.Mul Pos2 m m2
         , N.Mul Zero t Zero, N.Mul Pos1 t t
         , N.Sum l2 Zero l2
         , N.Sum Zero m2 m2,  N.Sum m2 Zero m2
         , N.Sum Zero t  t
         , N.Mul Pos3 i  il
         , N.Mul Pos1 i  i
         , N.Mul Neg2 i  it
         , N.Sum l2 il l'
         , N.Sum m2 i  m'
         , N.Sum t  it t'
         ) => Quantity (CGSDim l' m' t') a -> Quantity (Dim l m t i Zero Zero Zero) a
toSI x = x /~ unit_CGS (undefined :: Quantity (Dim l m t i Zero Zero Zero) a) *~ unit_SI' (undefined :: Quantity (Dim l m t i Zero Zero Zero) a)


-- -}

class SIEquivalent a d d' | d -> d' where -- 'd' is the SI dimension.
        unit :: Dimensional v d a -> Unit d' a

instance ( Floating a
         , N.Mul Zero l Zero, N.Mul Pos2 l l2
         , N.Mul Zero m Zero, N.Mul Pos2 m m2
         , N.Mul Zero t Zero, N.Mul Pos1 t t
         , N.Sum l2 Zero l2
         , N.Sum Zero m2 m2,  N.Sum m2 Zero m2
         , N.Sum Zero t  t
         , N.Mul Pos3 i  il
         , N.Mul Pos1 i  i
         , N.Mul Neg2 i  it
         , N.Sum l2 il l'
         , N.Sum m2 i  m'
         , N.Sum t  it t'
         ) => SIEquivalent a (Dim l m t i Zero Zero Zero) (CGSDim l' m' t') where
            unit _ = meter ^ (undefined :: l)
                   * SI.kilo gram ^ (undefined :: m)
                   * second       ^ (undefined :: t)
                   * ampere       ^ (undefined :: i)

toSI' :: ( SIEquivalent a (Dim l m t i Zero Zero Zero) (CGSDim l' m' t') ) => Quantity (CGSDim l' m' t') a -> Quantity (Dim l m t i Zero Zero Zero) a
toSI' x = x /~ unit    (undefined :: Quantity (Dim l m t i Zero Zero Zero) a)
            *~ unit_SI (undefined :: Quantity (Dim l m t i Zero Zero Zero) a)


{-
unit_CGS' :: ( Floating a, SIEquivalent (Dim l m t i Zero Zero Zero) (CGSDim l' m' t') )
          => Quantity (Dim l m t i Zero Zero Zero) a -> Unit (CGSDim l' m' t') a
unit_CGS' _ = meter        ^ (undefined :: l)
            * SI.kilo gram ^ (undefined :: m)
            * second       ^ (undefined :: t)
            * ampere       ^ (undefined :: i)
-}
