-- | This module describes internal states and transformations en those
--
-- Internal state should be kept in every part of the ject at its minimal and
-- thus there are multiple state datatypes for different parts of inerpreter
--
-- Transformations between different states should also be provided in this file
module CoALPj.InternalState (
	  CoALP
	, CoALPOptions
	, defaultCoALPOptions
	, caOptions
	, program
	, programPath
	, resolves
	, optVerbosity
	, REPLState
	, replInit
	, Verbosity(..)
	, runIO
	, iputStrLn
) where

import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.State (StateT) 
import Control.Monad.Trans.Except (ExceptT, throwE)

import Data.Monoid (mempty)

import System.IO.Error (tryIOError)


import CoALP.Error (Err(Msg))
import CoALP.Program (Program1, Succ1)


--
-- TODO refactor
-- 
--type CoALP = StateT IState (ErrorT IO)
--type CoALP = ErrorT Err IO
type CoALP = StateT REPLState (ExceptT Err IO)


iputStrLn :: String -> CoALP ()
iputStrLn s = runIO $ putStrLn s

-- | A version of liftIO that puts errors into the error type of the CoALPj monad
-- TODO is the use of ExceptT neccessary?
runIO :: IO a -> CoALP a
runIO x = lift $ liftIO (tryIOError x) >>= (either (throwE . Msg . show) return)


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
	, program 	:: Maybe Program1
	, programPath	:: Maybe FilePath
	, resolves	:: Maybe [Succ1]
	}

-- | Create initial state from general CoALPj options
replInit :: REPLState
--replInit caopts = REPLState {
replInit = REPLState {
	  caOptions = defaultCoALPOptions
	, program = mempty
	, programPath = mempty
	, resolves = mempty
	}

-- | Verbosity levels
data Verbosity = Quiet | Default | Verbose | VVerbose
	deriving (Eq, Ord)

