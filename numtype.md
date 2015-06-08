As of version 0.8 of the dimensional library the `Numeric.NumType` module was split off into its own package.

The numtype package provides unary type level representations of the (positive and negative) integers and basic operations (addition, subtraction, multiplication, division) on these. Due to the unary implementation the practical size of the NumTypes is severely limited making them unsuitable for large-cardinality applications. If you will be working with integers beyond (-20, 20) this package probably isn't for you. It is, however, eminently suitable for applications such as representing physical dimensions (as used in the dimensional library).

The numtype package can be installed using 'cabal install numtype'. See the [source](http://code.google.com/p/dimensional/wiki/SourceCode?tm=4) and [downloads](http://code.google.com/p/dimensional/downloads/list) tabs for other options.

The dimensional package depends on the numtype library. If you install dimensional off hackage with cabal-install this is transparent.