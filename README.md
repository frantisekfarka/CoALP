# CoALP

:copyright: 2014 - 2015: CoALP project, University of Dundee

License: LGPL v3


## Synopsis

Haskell implementation of coalgebraic logic programming. Experimental,
development version.


## Installation

### Dependencies

In order to draw diagrams correctly the implementation requires following 
libraries installed:

* ImageMagic
* Graphviz

The utilities @dot@ and @display@ should by visible in $PATH

### Developer's installation

Use cabal sandboxing feature:

> cabal sandbox init && cabal install --dependencies-only
> cabal run


### Standard installation using `cabal`

Run from the project directory:

> cabal install


### Installation with optional tests

Run from the project directory:

> cabal install --enable-tests

## Input format
Interpreter expect file whith extension /.logic/ that contains a set 
of clauses. Predicates can be marked inductive or coinductive. If unmarked
predicate is considered to be coinductive. Comments are denoted by a '%'.
An example of input program:

```
% nat_conat.logic
inductive : nat
nat(0).
nat(s(X)) :- nat(X).

% coinductive : conat
% the explicit marking is commented out, conat is considered coinductive
% implicitely
conat(0).
conat(s(X)) :- conat(X).
```


## Usage

Run the executable CoALPj with the parameter @--help@ to get the following
usage description:

```
CoALPj version 0.0.6, (C) 2014 - 2015

Usage: CoALPj [-v|--verbose] [-q|--quiet] [--gc3 ARG] [-V|--version]
  
Available options:
  -v,--verbose             Verbose output
  -q,--quiet               Suppress most of the output
  --gc3 ARG                Guardedness check of program ARG
  -V,--version             Print version information
  -h,--help                Show the help text
```

Running the executable without any parameters starts the interactive
interpreter. Further information can be accessed by typing @:help@


## Example programs

There are example programs in the following directories:

```
./examples
./examples/paper       -- examples programs in the paper
./examples/guarded     -- other guarded programs
./examples/unguarded   -- other unguarded programs
```

## Example of an interfactve session:


Let's load some program:

```
coinductive : nat
nat(n0).
nat(s(X)) :- nat(X).
```

which is in file paper/P1.logic.

```
$> :l examples/paper/P1.logic
Program examples/paper/P1.logic loaded.
```

Let's check that the program is guarded:
```
$> :gc3
True
```

And finaly, let's resolve some query:
```
$> answer :- nat(X).
True
        observed inductively: answer :- nat(n0).
```

Let's inspect other results till we hit the coinductive one:
```
$> ;
True
        observed inductively: answer :- nat(s(n0)).
...
$> ;
True
	observed co-inductively: answer :- nat(s(s(s(V_39)))).
			  GC: ( 1, s(V_2), [0,0]),

```

