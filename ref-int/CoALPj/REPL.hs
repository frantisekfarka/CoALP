-- | Read-Eval-Print loop
module CoALPj.REPL where

import Control.Exception (SomeException)
import Control.Monad (when)
import Control.Monad.Trans --(MonadTrans, lift)
--import Control.Monad.Trans.Except --(ExceptT (ExceptT), runExceptT, throwE)
-- | TODO possibly change ErrorT to ExceptT ?
import Control.Monad.Trans.Error
--import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
--import Control.Monad.Trans.Except
--import Control.Monad.Trans.State --(StateT, execStateT)
import qualified Control.Monad.Trans.Class as Trans --(lift)
import System.Console.Haskeline as H (
	  runInputT
	, catch
	, Settings (historyFile)
	, setComplete
	, defaultSettings
	, getInputLine
	, CompletionFunc
	, InputT (InputT)
	--, MonadException (controlIO )
	--, RunIO (RunIO )
	)
--import System.Console.Haskeline.MonadException
import System.IO ( BufferMode(LineBuffering), stdout, hSetBuffering)
import System.IO.Error (tryIOError)

import CoALPj.InternalState(
	  REPLState
	, CoALPOptions
	, replInit
	, caOptions
	, optVerbosity
	, Verbosity (..)
	)


--instance MonadException m => MonadException (ExceptT Err m) 
--instance MonadException CoALP

-- | Main CoALPj method
-- It's sepeared for main in order to allow different revocation modes
--
runMain :: CoALP () -> IO ()
runMain prog = do
	--res <- runEsceptT $ execStateT prog coalpInit
	res <- runErrorT $ prog 
	case res of
		Left err -> putStrLn $ "Uncaught error: " ++ show err
		Right _ -> return ()




-- | 
--
caMain :: CoALPOptions -> CoALP ()
caMain opts = do
	-- | TODO
	runIO $ hSetBuffering stdout LineBuffering
	let runrepl = True
	let histFile = ".history_file"
	let efile = ""
	let orig = replInit opts
	when runrepl $ do
		runInputT (replSettings (Just histFile))  $ repl orig efile

--
-- TODO refactor
-- 
--type CoALP = StateT IState (ErrorT IO)
type CoALP = ErrorT Err IO
data Err = EmptyMsg
	 | Msg String
         | InternalMsg String
	 | NotImplementedYet String
	deriving Show

instance Error Err where
	noMsg  = EmptyMsg
	strMsg = Msg



-- | A version of liftIO that puts errors into the error type of the CoALPj monad
-- TODO was used with ExceptT, is it neccessary?
runIO :: IO a -> CoALP a
runIO x = liftIO (tryIOError x) >>= either (throwError . Msg . show) return

-- :: Err -> CoALP a
--throwError = Trans.lift . throwE

replSettings :: Maybe FilePath -> Settings CoALP
replSettings hFile = setComplete replCompletion $ defaultSettings {
--replSettings hFile = setComplete replCompletion $ defaultSettings {
	historyFile = hFile 
	}

--repl orig efile = do
repl :: REPLState -> String -> InputT CoALP ()
repl orig efile = do
	let verbosity = optVerbosity $ caOptions orig
	let prompt = "$> "

	x <- H.catch (getInputLine prompt)
		(ctrlC (return Nothing))
	
	when (verbosity >= VVerbose) $ lift . iputStrLn $ show x
	case x of
		Nothing -> do
			lift $ when (verbosity >= Default) (iputStrLn "Bye bye")
			return ()
		Just input -> do
			-- | TODO implement
			repl orig efile
	where 
		ctrlC :: InputT CoALP a -> SomeException -> InputT CoALP a
		ctrlC act e = do
			Trans.lift $ iputStrLn (show e)
			act -- repl orig mods

-- | Complete REPL commands and defined identifiers
replCompletion :: CompletionFunc CoALP
replCompletion = error "completion is not implemented"

iputStrLn :: String -> CoALP ()
iputStrLn s = runIO $ putStrLn s

--instance (MonadIO m) => MonadIO (ExceptT Err m) where
--    liftIO = lift . liftIO
