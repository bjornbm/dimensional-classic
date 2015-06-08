GHC 7.0.1 has a [bug](http://www.haskell.org/pipermail/glasgow-haskell-users/2011-January/019910.html) related to type-inferencing that can cause programs to loop (hang). A minimal example to induce the bug is:

```
sqrt (9 *~ meter^pos2)
```

A work-around (for this particular case) is to add a type signature to the argument of `sqrt`:

```
sqrt (9 *~ meter^pos2 :: Area Double)
```

This bug was [fixed](http://www.haskell.org/pipermail/glasgow-haskell-users/2011-January/019916.html) in GHC 7.0.2. (Thanks Pavel Perikov for reporting this!)