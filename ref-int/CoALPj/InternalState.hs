-- | This module describes internal states and transformations en those
--
-- Internal state should be kept in every part of the ject at its minimal and
-- thus there are multiple state datatypes for different parts of inerpreter
--
-- Transformations between different states should also be provided in this file
module CoALPj.InternalState (
	  CoALPOptions
	, defaultCoALPOptions
	, caOptions
	, program
	, optVerbosity
	, REPLState
	, replInit
	, Verbosity(..)
) where

import Data.Monoid (mempty)

import CoALP.Program (Program)


-- | General CoALPj options that affect all code
data CoALPOptions = CoALPOptions {
	  optVerbosity :: Verbosity
	}

-- | default CoALPj options
defaultCoALPOptions :: CoALPOptions
defaultCoALPOptions = CoALPOptions {
	  optVerbosity = Default
	}

-- | Read-Eval-Print loop state
data REPLState = REPLState {
	  caOptions 	:: CoALPOptions
	, program 	:: Program
	}

-- | Create initial state from general CoALPj options
replInit :: REPLState
--replInit caopts = REPLState {
replInit = REPLState {
	  caOptions = defaultCoALPOptions
	, program = mempty
	}

-- | Verbosity levels
data Verbosity = Quiet | Default | Verbose | VVerbose
	deriving (Eq, Ord)

