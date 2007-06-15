
--    *** EXPERIMENTAL ***

{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module CGS where

import Prelude ( undefined, Num, Fractional, Show )
import qualified Prelude
import Buckwalter.Dimensional
import qualified Buckwalter.Dimensional.SIUnits as SI
import qualified Buckwalter.NumType as N
import Buckwalter.NumType ( Zero, Pos, Pos1, Pos2, NumType )
import Buckwalter.NumType ( zero, pos1, pos2 )


data CGSDim lh 
instance Show (CGSDim lh) where show _ = "CGS"

instance (N.Sum lh lh' lh'') => Mul  (CGSDim lh) (CGSDim lh') (CGSDim lh'')
instance (N.Sum lh lh' lh'') => Div  (CGSDim lh'') (CGSDim lh') (CGSDim lh)
instance (N.Mul lh x lh')    => Pow  (CGSDim lh) x (CGSDim lh')
instance (N.Div lh x lh')    => Root (CGSDim lh) x (CGSDim lh')

meter :: Num a => Unit (CGSDim Pos2) a
meter = Dimensional 100
gram :: Num a => Unit (CGSDim Pos1) a
gram = Dimensional 1


-- fromSI :: forall a l m l2 mn lh. 
--        ( Fractional a, NumType l, NumType m, NumType lh
--        , N.Mul Zero l Zero, N.Mul (Pos Zero) l l, N.Mul Pos2 l l2
--        , N.Mul Zero m Zero, N.Mul (Pos Zero) m m
--        , N.Sum l Zero l
--        , N.Negate m mn, N.Sum m mn Zero
--        , N.Sum l2 m lh
--        ) 
{-
fromSI :: forall a l m lh l2 mn. 
        ( Fractional a, NumType l, NumType m 
        , N.Mul Zero l Zero, N.Mul (Pos Zero) l l
        , N.Mul Zero m Zero, N.Mul (Pos Zero) m m
        , N.Sum l Zero l
        , N.Negate m mn, N.Sum m mn Zero
        , N.Mul Pos2 l l2
        , N.Sum l2 m lh
        ) 
       => Quantity (Dim l m Zero Zero Zero Zero Zero) a -> Quantity (CGSDim lh) a
-- -}
fromSI x = x /~ unit_SI x *~ unit_CGS x

unit_SI :: forall a l m .
        ( Fractional a
        , N.Mul Zero l Zero, N.Mul Pos1 l l
        , N.Mul Zero m Zero, N.Mul Pos1 m m
        , N.Sum l Zero l
        , N.Sum Zero m m
        )
        => Quantity (Dim l m Zero Zero Zero Zero Zero) a -> Unit (Dim l m Zero Zero Zero Zero Zero) a
unit_SI _ = SI.meter ^ (undefined :: l) * SI.kilo SI.gram ^ (undefined :: m)

unit_CGS :: forall a l m lh l2.
         ( Fractional a
         , N.Mul Pos2 l l2
         , N.Mul Pos1 m m
         , N.Sum l2 m lh
         ) => Quantity (Dim l m Zero Zero Zero Zero Zero) a -> Unit (CGSDim lh) a
unit_CGS _ = meter ^ (undefined :: l) * SI.kilo gram ^ (undefined :: m)
