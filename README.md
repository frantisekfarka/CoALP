# CoALP

:copyright: 2014: CoALP project, University of Dundee

License: LGPL v3


## Synopsis

Haskell implementation of coalgebraic logic programming. Experimental,
development version.


## Developer's installation

Use cabal sandboxing feature:

> cabal sandbox init && cabal build --dependencies-only
> caba run


### Standard installation using `cabal`

Run from the project directory:

> cabal install


### Installation with optional tests

Run from the project directory:

> cabal install --enable-tests
