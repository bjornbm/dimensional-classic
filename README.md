# dimensional

Dimensional is a library providing data types for performing
arithmetic with physical quantities and units. Information about
the physical dimensions of the quantities/units is embedded in their
types and the validity of operations is verified by the type checker
at compile time. The boxing and unboxing of numerical values as
quantities is done by multiplication and division with units. The
library is designed to, as far as is practical, enforce/encourage
[best practices](http://physics.nist.gov/Pubs/SP811/) of unit usage.

A spin-off of dimensional is the [numtype](numtype) library which
provides a type-level representation of integers. Numtype is a
dependency of the dimensional library.

For documentation: see _Conclusion and usage_ in the [original
announcement](http://www.haskell.org/pipermail/haskell/2006-December/018993.html),
the literate haskell code or the examples in the wiki.

## Installation

The latest release can be installed with `cabal install dimensional` or
downloaded from [Hackage](http://hackage.haskell.org/).


## Other implementations

This implementation of dimensional, built on multiparameter type classes
and functional dependencies, is the canonical version.

### [dimensional-tf](https://github.com/bjornbm/dimensional-tf/)

In addition to the standard dimensional built on Functional Dependencies
there is an experimental implementation of dimensional built on (open)
Type Families. Install with `cabal install dimensional-tf`.


### [dimensional-dk](https://github.com/bjornbm/dimensional-dk/) (the
future)

A new implementation leveraging Data Kinds and Closed Type Families is
being drafted on [Github](https://github.com/bjornbm/dimensional-dk/).
We believe this will be the new preferred implementation once it is
completed.


---

[![](http://api.flattr.com/button/flattr-badge-large.png)](http://flattr.com/thing/158745/Dimensional-library-statically-checked-physical-dimensions-for-Haskell-)
