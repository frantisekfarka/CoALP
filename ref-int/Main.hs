module Main where

--import Prelude () 
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.State (StateT, execStateT)


import CoALPj.CmdOpts (Opt, runArgParser)

main :: IO ()
main = do 
	opts <- runArgParser
	print opts
	return ()




	--runMain (runCoALPj opts)





--
-- TODO refactor
--
runMain :: Idris () -> IO ()
runMain prog = do
	res <- runExceptT $ execStateT prog coalpInit
	case res of
		Left err -> putStrLn $ "Uncaught error: " ++ show err
		Right _ -> return ()

runCoALPj :: [Opt] -> Idris ()
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

coalpInit = undefined

type Idris = StateT IState (ExceptT Err IO)
data IState = IState
data Err = Err
	deriving Show


