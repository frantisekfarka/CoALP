-- | Read-Eval-Print loop
module CoALPj.REPL where

import Control.Exception (SomeException)
import Control.Monad (when)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, mapExceptT, throwE)
import Control.Monad.Trans.State --(StateT, execStateT)
import Data.Functor.Identity (runIdentity)

import System.Console.Haskeline as H (
	  runInputT
	, catch
	, Settings (historyFile)
	, setComplete
	, defaultSettings
	, getInputLine
	, InputT 
	)
import System.Console.Haskeline.Completion (Completion(..), CompletionFunc)
import System.Console.Haskeline.MonadException (MonadException (controlIO), RunIO (RunIO))

import System.IO ( BufferMode(LineBuffering), stdout, hSetBuffering)
import System.IO.Error (tryIOError)

import CoALPj.InternalState(
	  REPLState
	, CoALPOptions
	, replInit
	, caOptions
	, program
	, optVerbosity
	, Verbosity (..)
	)
import CoALPj.REPL.Commands(
	  Command(..)
	)
import CoALPj.REPL.Parser(
	  parseCmd
	)

import CoALP.Error (Err(..))

-- TODO refactor
import CoALP.Parser.Lexer
import CoALP.Parser.Parser (parse)

import CoALP.Parser.PrettyPrint (ppProgram)

-- | MonadException instance for ExceptT
-- this is only a substitution for missing instance in haskeline-1.7.3
-- this instance can be removed once the haskeline is updated
instance (MonadException m) => MonadException (ExceptT e m) where
	controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
		run' = RunIO (fmap ExceptT . run . runExceptT)
		in fmap runExceptT $ f run'

instance MonadException m => MonadException (StateT s m) where
	controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
		run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
		in fmap (flip runStateT s) $ f run'


-- | Main CoALPj method
	-- It's sepeared from main in order to allow different revocation modes
--
runMain :: CoALP () -> IO ()
runMain prog = do
	res <- runExceptT $ execStateT prog replInit
	case res of
		Left err -> putStrLn $ "Uncaught error: " ++ show err
		Right _ -> return ()

-- | CoALPj main loop initialization
--
caMain :: CoALPOptions -> CoALP ()
caMain opts = do
	s <- get
	put $ s { caOptions = opts }
	-- | TODO
	runIO $ hSetBuffering stdout LineBuffering
	let runrepl = True
	let histFile = ".history_file"
	let efile = ""
	let orig = replInit 
	when runrepl $ do
		runInputT (replSettings (Just histFile))  $ repl orig efile

--
-- TODO refactor
-- 
--type CoALP = StateT IState (ErrorT IO)
--type CoALP = ErrorT Err IO
type CoALP = StateT REPLState (ExceptT Err IO)



-- | A version of liftIO that puts errors into the error type of the CoALPj monad
-- TODO is the use of ExceptT neccessary?
runIO :: IO a -> CoALP a
runIO x = lift $ liftIO (tryIOError x) >>= (either (throwE . Msg . show) return)

replSettings :: Maybe FilePath -> Settings CoALP
replSettings hFile = setComplete replCompletion $ defaultSettings {
	  historyFile = hFile 
	}

-- | Read -- Eval -- Print Loop
--   from initial state
repl :: REPLState -> String -> InputT CoALP ()
repl initState efile = do
	let verbosity = optVerbosity $ caOptions initState
	-- TODO use prompt from state
	let prompt = "$> "

	x <- H.catch (getInputLine prompt)
		(ctrlC (return Nothing))
	
	when (verbosity >= VVerbose) $ lift . iputStrLn $ show x
	case x of
		Nothing -> do
			-- TODO refactor string to some other place
			lift $ when (verbosity >= Default) (iputStrLn "Bye bye")
			return ()
		Just input -> do
			-- | TODO catch process errors properly
			-- refactor this hack
			--lift $ processInput input initState
			catch (lift $ processInput input initState)
                                (ctrlC (return ()))
			--res <- lift $ processInput input initState
			--case res of 
			--	Right m		-> lift $ return m
			--	Left err	-> lift $ iputStrLn $ show err
			repl initState efile
	where 
		ctrlC :: InputT CoALP a -> SomeException -> InputT CoALP a
		ctrlC act e = do
			lift $ iputStrLn (show e)
			act -- repl orig mods
		inputExcept :: SomeException -> InputT CoALP ()
		inputExcept e = do
			lift $ iputStrLn (show e)
			return ()

-- | Complete REPL commands and defined identifiers
-- TODO proper implemnetation
replCompletion :: CompletionFunc CoALP
replCompletion (prev, next) = return ( "", fmap compl [
		  " -- TODO implement completion"
		, " -- you can always try other comletions ..."
		])
	where
		compl x = Completion {
			  replacement = x
			, display = "try " ++ x
			, isFinished = False
			}

iputStrLn :: String -> CoALP ()
iputStrLn s = runIO $ putStrLn s

-- | Process REPL command line input
processInput :: String -> REPLState -> CoALP ()
processInput cmd origState = do
	-- some initialization?
	case parseCmd cmd of
		Left err 	-> do
			iputStrLn $ show err
			return ()

		Right (Load f)	-> do
			loadFile f

		Right (Print)	-> do
			printProgram

		Right a 	-> do
			iputStrLn $ "doing some action: " ++ (show a)
			return ()

-- | load and parse file
loadFile :: FilePath -> CoALP ()
loadFile file = do
	cnt <- lift . lift $ readFile file
	--iputStrLn $ ppLexer $ scanTokens cnt
	--TODO refactor parser monad (stack)
	case parse cnt of
		Left err	-> do
			iputStrLn err
			return ()
		Right prg	-> do
			iputStrLn $ "Program " ++ file ++ " loaded."
			--iputStrLn . ppProgram $ prg
			s <- get
			put $ s { program = prg }
			
-- | print program
printProgram :: CoALP ()
printProgram = get >>= iputStrLn . ppProgram . program 
			

