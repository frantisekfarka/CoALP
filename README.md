# CoALP

:copyright: 2014 - 2015: CoALP project, University of Dundee

License: LGPL v3


## Synopsis

Haskell implementation of coalgebraic logic programming. Experimental,
development version.


## Instalation

### Dependencies

In order to draw diagrams correctly the implementation requires following 
libraries installed:

* ImageMagic
* Graphviz

The utilites @dot@ and @display@ should by visible in $PATH

### Developer's installation

Use cabal sandboxing feature:

> cabal sandbox init && cabal install --dependencies-only
> caba run


### Standard installation using `cabal`

Run from the project directory:

> cabal install


### Installation with optional tests

Run from the project directory:

> cabal install --enable-tests


## Usage

CoALPj version 0.0.6, (C) 2014 - 2015

```
Usage: CoALPj [-v|--verbose] [-q|--quiet] [--gc3 ARG] [-V|--version]
  
Available options:
  -v,--verbose             Verbose output
  -q,--quiet               Suppres most of the output
  --gc3 ARG                Guardednes check of program ARG
  -V,--version             Print version information
  -h,--help                Show this help text
```

## Example programs
