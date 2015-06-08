# Introduction #

I received a question from Daniel Buckmaster:

> Did you consider adding inaccuracy to units? For example,
```
height = 10 *~ meter +- 1
```
> Presumably then inaccuracies could be dealt with automatically over calculations.


# Answer #

Actually, by having quantities being based on instances of Num you have a lot of flexibility in terms of what you can do with them. If you find or write a library that adds inaccuracy to Num:s you automatically can leverage that when working with dimensional. For example, you could write (assuming normal (gaussian) distribution of the inaccuracy):

```
module Uncertain (Uncertain (..)) where

rss :: Floating a => a -> a -> a
rss x y = sqrt (x^2 + y^2)

data Uncertain a = a :+- a deriving Show

instance (Floating a, Eq a) => Num (Uncertain a) where
  (x:+-dx) + (y:+-dy) = (x + y) :+- rss dx dy
  (x:+-dx) - (y:+-dy) = (x - y) :+- rss dx dy
  fromInteger i = fromInteger i :+- 0
  -- Multiplication works unless both values are uncertain.
  (x:+- 0) * (y:+-dy) = (x * y) :+- (x * dy)
  (x:+-dx) * (y:+- 0) = (x * y) :+- (dx * y)
  _ * _  = error "*"      -- The result will be a chi^2(?) distribution, for
                          -- which this representation is insufficient.
  abs    = error "abs"    -- Not obvious what to do here.
  signum = error "signum" -- Not obvious what to do here.
```

Then you could use that with dimensional, e.g.:

```
import Numeric.Units.Dimensional.Prelude
import Uncertain
import qualified Prelude

height = (10 :+- 1) *~ meter
x = (2 :+- 0.2) *~ meter

main = print $ height + x  -- "12.0 :+- 1.019803902718557 m"
```

That being said, this representation of uncertainty breaks down pretty rapidly as the gaussian distribution does not translate very far beyond addition and subtraction (as you can see in my Num instance). For this particular problem something more clever (or brute force, e.g. Monte Carlo method) is needed—if you look around on Hackage you may find that someone has solved this problem already for Num (et al)!

There are some disadvantages to wrapping an arbitrary Num instance in Dimensional (the type variable being one of them), but the flexibility you gain and the fact that you can leverage existing work on the Num instance makes it a win in my opinion.

Good luck!