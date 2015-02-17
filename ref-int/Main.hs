module Main where

--import Prelude () 
import System.Exit (exitWith, ExitCode(ExitSuccess))

import CoALPj.CmdOpts (CmdOpts, runArgParser, optDummy1)
import CoALPj.REPL (runMain, CoALP, runIO, caMain)


main :: IO ()
main = do 
	opts <- runArgParser
	runMain (runCoALPj opts)




-- | runCoALPj enables to process parameters or run different actions insted of
-- manin REPL loop (e. g. some package processing?)
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
		0          -> do
			runIO $ putStrLn "Dummy Bye Bye ..."
			runIO $ exitWith ExitSuccess
		otherwise  -> return ()
	caMain opts





