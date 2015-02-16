-- | Read-Eval-Print loop
module CoALPj.REPL where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.State (StateT, execStateT)
import Data.Version (showVersion)

import Paths_CoALP
import Version_CoALP

-- | Main CoALPj method
-- It's sepeared for main in order to allow different revocation modes
--
runMain :: CoALP () -> IO ()
runMain prog = do
	res <- runExceptT $ execStateT prog coalpInit
	case res of
		Left err -> putStrLn $ "Uncaught error: " ++ show err
		Right _ -> return ()

ver = showVersion version ++ gitHash


--
-- TODO refactor
-- 
type CoALP = StateT IState (ExceptT Err IO)
data Err = Err
	deriving Show

coalpInit = undefined

data IState = IState