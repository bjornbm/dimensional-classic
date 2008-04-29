Test the ForwardAD module.

> import Numeric.Units.Dimensional.Prelude
> import Numeric.Units.Dimensional.ForwardAD
> import qualified Prelude
> import Test.QuickCheck

Our "initial values".

> v :: Fractional a => Velocity a
> v = 3 *~ (meter / second)
> a :: Fractional a => Acceleration a
> a = 2 *~ (meter / second ^ pos2)

Distance and its derivatives calculated by hand. Not a very solid
example. Should add e.g. trig functions.

> distance :: Fractional a => Time a -> Length a
> distance t = v * t + a * t ^ pos2
> distance' :: Fractional a => Time a -> Velocity a
> distance' t = v + _2 * a * t
> distance'' :: Fractional a => Time a -> Acceleration a
> distance'' t = _2 * a

QuickCheck properties.

> prop_vel  t = diff distance t' == distance' t'
>   where t' = t *~ second :: Time Double
> prop_acc1 t = diff (diff distance) t' == distance'' t'
>   where t' = t *~ second :: Time Double
> prop_acc2 t = diff distance' t' == distance'' t'
>   where t' = t *~ second :: Time Double

Driver.

> main = do
>   quickCheck prop_vel
>   quickCheck prop_acc1
>   quickCheck prop_acc2

