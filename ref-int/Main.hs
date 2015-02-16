module Main where

--import Prelude () 


import CoALPj.CmdOpts (CmdOpts, runArgParser)
import CoALPj.REPL (runMain, CoALP)

main :: IO ()
main = do 
	opts <- runArgParser
	runMain (runCoALPj opts)





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
	undefined





