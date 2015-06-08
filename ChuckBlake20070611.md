
```
                                                                                                                                                                                                Date: Mon, 11 Jun 2007 14:17:36 -0400
From: Chuck Blake
To: bjorn.buckwalter
Subject: units library in Haskell

Hey...I was just reading your 0.5 version.  It looks pretty nice.
I had a constructive comment.

Rational exponents of basic dimensions are perfectly reasonable
and arise all the time, often to avoid creating new dimensions
for some new kind of thing or to make some physical constant
dimensionless (and often 1 or 2 pi or something).

This is what you call "unit models" (after tha Java stuff), though
I think of them as "systems" after the way "SI" is named.
First, a concrete example.  In CGS units the dimensions for
electric charge have half integral exponents --
    charge ~ gram^(1/2) * cm ^ (3/2) * s^-1.

Why?  Just "back them out" of equations with integral exponents...
in this case you could use Coulomb's Law: 
    F = ma = constant * q_1 * q_2 / r^2

Regardless of the value of constant, if we want it to be dimensionless
then the q's have half-integral exponents of the base dimensions.

    mass * length / time^2 ~ (units of q)^2 / (length^2)
or
    mass * length^3 / time^2 ~ (units of q)^2

simply because the left hand side has odd powers of mass and
length when we take the square root we get the half-integers.

So, the point of the elaboration here is that whether you have
rational arithmetic in exponents or just integer arithmetic is
mostly a matter of whether you create new dimensions in your
unit space for "new things" that can be meaningfully multiplied
like the q's in the above case.

A competing tension is the various unit systems.  Often one
reduces the number of base dimensions to 1-D in order to make
as many as possible physical constants = 1 and thus go away
in complex equations.  The more dimensions you make vanish,
the more likely various fractional exponents occur.  You can
easily get 1/3's in there if volumetric equations are involved,
or occasionally even quarter-powers with things like radio
ranging or black body radiation and so on.  So, 2/3 and 3/4
and all that come up.  So, you probably need full-on rational
arithmetic on exponents. [ Strictly speaking real arithmetic,
but exponents like pi are really quite rare almost always. ]

So, obviously your package is currently mostly trying to just
get some solid SI unit thing going which is fine and great.
I'm not enough of a Haskell hacker to suggest *how* or even if
handling a varying number of base dimensions should/could be done.

Essentially, one can think of the dimensions themselves as a vector
space (7-D, 3-D, whatever).  Multiplying numbers is like adding in
the vector space.  Dividing like subtracting.  Taking powers is like
multiplication by a scalar, and taking square roots (also fine) is
like multiplication by a fraction.  So, there is no need for nodes
in the type lattice to be vectors of integers if you allow any "new"
multiplicative combinations to be given units.

Anyway, conceiving a dimension systme like a vector space allows
me to say that those 1-D "natural unit" type systems are projections
onto a 1-D subspace.  There are various projections...sometimes
onto just a plane or 3-D subspace and sometims onto 1-D.

It would be great, though perhaps hard?, to provide "unit system
conversions".  There you want to say type check and convert from
SI units to natural units.  The projection is unambiguous, so
this is possible in theory anyway.  Reverse-projecting requires
the user to specify the target units in the higher dimension to
disambiguate, of course -- go to -> SI.Energy (from some expression
of type Natural.InverseLength).

Those cross-system conversions and type checking are even *more*
of a royal pain than regular ones as they involve bits of linear
algebra on the dimensions (albeit typically 3-vectors or something)
as well as the usual mess.  So it would be fabulous to do this
automatically.  You could easily imagine some relativity functions
that just do c=1 and general relativity functions that do both c=1
and G=1 and so on...just as an example...It would be so nice to
have a type system that could "do the right thing".

Anyway, just some thoughts for the ultimate programming language
unit type system!  I think you're off to a great start, though.
Just constructive thoughts if you haven't thought about them much.

I think Haskell may be the first language with a sufficiently
expressive static type system to do all this at compile time.
Mathematica is dynamic and the dynamic case is far easier -- you
just bundle values with a variable-length vector and a tag
identifying which projection that vector is.

cb
```



