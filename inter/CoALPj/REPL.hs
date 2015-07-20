{-# LANGUAGE FlexibleContexts #-}
-- | Read-Eval-Print loop
module CoALPj.REPL (
	  runMain
	, caMain
	)where

import Control.Exception (SomeException,fromException)
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Trans.State (StateT (..), execStateT, get, put)
--import Data.Functor ((<$>))

import System.Console.Haskeline as H (
	  runInputT
	, catch
	, Settings (historyFile)
	, setComplete
	, defaultSettings
	, getInputLine
	, InputT 
	)
import System.Console.Haskeline.MonadException (MonadException (controlIO), RunIO (RunIO))

import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.IO ( BufferMode(LineBuffering), stdout, hSetBuffering)

import CoALPj.InternalState (
	  CoALP
	, REPLState
	, CoALPOptions
	, replInit
	, caOptions
	, optVerbosity
	, Verbosity (..)
	, runIO
	, iputStrLn
	)
import CoALPj.REPL.Commands (
	  Command(..)
	)
import CoALPj.REPL.Parser (
	    parseCmd
	  , cmdInfo
	  , replCompletion
	)

import CoALPj.Actions (
	  loadFile
	, reloadFile
	, resolve
	, printProgram
	, checkGuard1
	, checkGuard2
	, checkGuard3
	, checkGuard3One
	, drawProgram
	, drawRew
	, drawTrans
	, drawDer
	, drawInf
	, drawUng
	)

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
	iputStrLn replWelcome
	when runrepl $ do
		runInputT (replSettings (Just histFile))  $ repl orig efile

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
			lift $ when (verbosity >= Default) (iputStrLn replBye)
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
		ctrlC act e = case fromException e of
			Just ExitSuccess	-> do
				lift . iputStrLn $ replBye
				lift . lift . lift $ exitWith ExitSuccess
			_			-> do
				lift $ iputStrLn ("Err " ++ show e)
				act -- repl orig mods
		{-inputExcept :: SomeException -> InputT CoALP ()
		inputExcept e = do
			lift $ iputStrLn (show e)
			return ()
		-}

replWelcome :: String
replWelcome =
  "░█▀▀░░░░░█▀█░█░░░█▀█░░░░▀░░█▀█░▀█▀░█▀▀░█▀▄░█▀█░█▀▄░█▀▀░▀█▀░█▀▀░█▀▄\n" ++
  "░█░░░█▀█░█▀█░█░░░█▀▀░░░░█░░█░█░░█░░█▀▀░█▀▄░█▀▀░█▀▄░█▀▀░░█░░█▀▀░█▀▄\n" ++
  "░▀▀▀░▀▀▀░▀░▀░▀▀▀░▀░░░░░░█░░▀░▀░░▀░░▀▀▀░▀░▀░▀░░░▀░▀░▀▀▀░░▀░░▀▀▀░▀░▀\n" ++
  "░░░░░░░░░░░░░░░░░░░░░░░▀ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░\n\n" ++
  "(C) 2014 - 2015, University of Dundee\n\n" ++
  "Type \":help\" for usage information.\n"

replBye :: String
replBye = "Bye bye\n" ++
	" ______\n" ++
	"( Bye. )\n" ++
	" ------\n" ++
	"         \\   (__)\n" ++
	"          \\  (oo)\\_______\n" ++
	"             (__)\\       )\\\n" ++
	"                 ||----w |!\n" ++
	"                 ||     ||"


-- | Process REPL command line input
processInput :: String -> REPLState -> CoALP ()
processInput cmd _origState = do
	-- some initialization?
	case parseCmd cmd of
		Left err 	-> do
			iputStrLn $ show err
			return ()
		Right (Load f)	-> loadFile f
		Right (Reload)	-> reloadFile 
		Right (Print)	-> printProgram

		Right (Quit) 	-> do
			-- iputStrLn $ "doing some action: " ++ (show a)
			-- return ()
			_ <- runIO $ exitWith ExitSuccess
			undefined
		Right (GC1)	-> checkGuard1
		Right (GC2 c)	-> checkGuard2 c
		Right (GC3)	-> checkGuard3 
		Right (GC3One c) -> checkGuard3One c

		Right (DrawProgram) -> drawProgram
		Right (DrawRew d q) -> drawRew d q
		Right (DrawTrans d v q) -> drawTrans d v q
		Right (DrawDer dD dR q) -> drawDer dD dR q
		Right (DrawInf dD dR q) -> drawInf dD dR q
		Right (DrawUng dD dR q) -> drawUng dD dR q
		Right (Help) -> iputStrLn cmdInfo
		Right (Empty) -> return ()
		Right (Resolve c) -> resolve c


