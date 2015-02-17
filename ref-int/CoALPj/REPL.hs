-- | Read-Eval-Print loop
module CoALPj.REPL where

import Control.Exception (SomeException)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (StateT, execStateT)
import qualified Control.Monad.Trans.Class as Trans (lift)
import System.Console.Haskeline as H (
	  runInputT
	, catch
	, Settings (historyFile)
	, setComplete
	, defaultSettings
	, getInputLine
	, CompletionFunc
	, InputT
	)
--import System.Console.Haskeline.MonadException
import System.IO ( BufferMode(LineBuffering), stdout, hSetBuffering)
import System.IO.Error (tryIOError)

import CoALPj.CmdOpts (CmdOpts)

-- | Main CoALPj method
-- It's sepeared for main in order to allow different revocation modes
--
runMain :: CoALP () -> IO ()
runMain prog = do
	res <- runExceptT $ execStateT prog coalpInit
	case res of
		Left err -> putStrLn $ "Uncaught error: " ++ show err
		Right _ -> return ()




-- | 
--
caMain :: CmdOpts -> CoALP ()
caMain opts = do
	

	-- | TODO
	runIO $ hSetBuffering stdout LineBuffering
	let runrepl = True
	let historyFile = "history_file"
	let efile = ""
	orig <- return IState --_ -- getIState
	when runrepl $ do
		--startServer port orig mods
		--runInputT (replSettings (Just historyFile)) $ repl orig efile
		undefined

--
-- TODO refactor
-- 
type CoALP = StateT IState (ExceptT Err IO)
data Err = Msg String
         | InternalMsg String
	deriving Show

coalpInit = undefined

data IState = IState


-- | A version of liftIO that puts errors into the exception type of the CoALPj monad
runIO :: IO a -> CoALP a
runIO x = liftIO (tryIOError x) >>= either (throwError . Msg . show) return

throwError :: Err -> CoALP a
throwError = Trans.lift . throwE

{-replSettings :: Maybe FilePath -> Settings CoALP
replSettings hFile = setComplete replCompletion $ defaultSettings {
	historyFile = hFile 
	}
-}

repl orig efile = do
	let quiet = False
	let prompt = "$ >"
	undefined
{-
	x <- H.catch (getInputLine prompt)
		(ctrlC (return Nothing))
	case x of
		Nothing -> do
			lift $ when (not quiet) (iputStrLn "Bye bye")
			return ()
		Just input -> do
			-- | TODO implement
			Trans.lift $ iputStrLn input
	where 
		ctrlC :: InputT CoALP a -> SomeException -> InputT CoALP a
		ctrlC act e = do
			undefined
			--Trans.lift $ iputStrLn (show e)
			--act -- repl orig mods
-}

-- | Complete REPL commands and defined identifiers
replCompletion :: CompletionFunc CoALP
replCompletion = undefined

iputStrLn :: String -> CoALP ()
iputStrLn s = runIO $ putStrLn s
