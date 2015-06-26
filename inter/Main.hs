module Main where

import Control.Monad.Trans.State (get, put)
--import Prelude () 
import System.Exit (exitWith, ExitCode(ExitSuccess))

import CoALPj.Actions (loadFile, checkGuard3)
import CoALPj.CmdOpts (CmdOpts, runArgParser, optDummy1, optVerbose, optVVerbose, optQuiet, optGC3)
import CoALPj.REPL (runMain, caMain)
import CoALPj.InternalState (defaultCoALPOptions, optVerbosity, Verbosity(..), CoALP, runIO, REPLState(..))

main :: IO ()
main = do 
	opts <- runArgParser
	runMain (runCoALPj opts)

-- | runCoALPj enables to process parameters or run different actions insted of
-- main REPL loop (e. g. some package processing?)
runCoALPj :: CmdOpts -> CoALP ()
runCoALPj opts = do
	--when (ShowIncs `elem` opts) $ runIO showIncs
	--case opt getClient opts of
	--	[]    -> return ()
	--	(c:_) -> do
	--		setVerbose False
	--		setQuiet True
	--		runIO $ runClient (getPort opts) c
	--		runIO $ exitWith ExitSuccess
	case optDummy1 opts of
		0  -> do
			runIO $ putStrLn "Dummy Bye Bye ..."
			runIO $ exitWith ExitSuccess
		_  -> return ()
	case optGC3 opts of
		Just fp  -> do
			silence
			loadFile fp
			checkGuard3
			runIO $ exitWith ExitSuccess
		Nothing -> return ()
	caMain caOpts
	where
		silence = get >>= \x -> put (
			x { caOptions = (caOptions x) { optVerbosity = Quiet}} )
		caOpts = defaultCoALPOptions {
			  optVerbosity = verb
			}
		-- TODO reafactor!!
		verb = case (optVVerbose opts, optVerbose opts, optQuiet opts) of
			(True  , _    , _   ) -> VVerbose
			(False , True , _   ) -> Verbose
			(False , False, True) -> Quiet
			_                     -> Default


