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
	, optVerbosity
	, REPLState
	, replInit
	, Verbosity(..)
) where


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
	  caOptions :: CoALPOptions
	}

-- | Create initial state from general CoALPj options
replInit :: CoALPOptions -> REPLState
replInit caopts = REPLState {
	  caOptions = caopts
	}

-- | Verbosity levels
data Verbosity = Quiet | Default | Verbose | VVerbose
	deriving (Eq, Ord)

