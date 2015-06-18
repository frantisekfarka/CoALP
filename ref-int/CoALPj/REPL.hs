{-# LANGUAGE FlexibleContexts #-}
-- | Read-Eval-Print loop
module CoALPj.REPL where

import Control.Exception (SomeException)
import Control.Monad (when, (=<<))
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, mapExceptT, throwE)
import Control.Monad.Trans.State --(StateT, execStateT)
import Control.Monad.State (MonadState)
import Control.Monad.IO.Class(MonadIO)
import Data.Functor ((<$>))
import Data.Functor.Identity (runIdentity)
import Data.Maybe (maybe)

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

import System.IO ( BufferMode(LineBuffering), stdout, hSetBuffering)
import System.IO.Error (tryIOError)

import CoALPj.InternalState(
	  REPLState
	, CoALPOptions
	, replInit
	, caOptions
	, program
	, programPath
	, optVerbosity
	, Verbosity (..)
	)
import CoALPj.REPL.Commands(
	  Command(..)
	)
import CoALPj.REPL.Parser(
	    parseCmd
	  , cmdInfo
	  , replCompletion
	)

import CoALP.Error (Err(..))

import CoALP.Render (renderProgram,displayProgram,displayRewTree)
import CoALP.Guards2 (gc1,gc2,loops')
import CoALP.Program (Program1,fixQuery)

import CoALP.RewTree (rew, trans,mkVar)


-- TODO refactor
import CoALP.Parser.Lexer
import CoALP.Parser.Parser (parse,parseQuery)

import CoALP.Parser.PrettyPrint (ppProgram)
import CoALP.Program (Clause(..))

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
		Right (Load f)	-> loadFile f
		Right (Reload)	-> reloadFile 
		Right (Print)	-> printProgram

		Right (Quit) 	-> do
			-- iputStrLn $ "doing some action: " ++ (show a)
			-- return ()
			lift $ throwE QuitErr
			undefined
		Right (GC1)	-> checkGuard1
		Right (GC2 q)	-> checkGuard2 q

		Right (DrawProgram) -> drawProgram
		Right (DrawRew d q) -> drawRew d q
		Right (DrawTrans d v q) -> drawTrans d v q
		Right (DrawDer dD dR q) -> drawDer dD dR q
		Right (Help) -> iputStrLn cmdInfo
		Right (Empty) -> return ()

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
			put $ s { program = Just (reverse prg), programPath = Just file }

reloadFile :: CoALP ()
reloadFile = maybe (iputStrLn "No program loaded yet") loadFile
	=<< programPath <$> get
	
			
-- | print program
printProgram :: CoALP ()
--printProgram = get >>= iputStrLn . ppProgram . program 
printProgram = whenProgram (iputStrLn . ppProgram)
		
checkGuard1 :: CoALP ()
checkGuard1 = whenProgram (iputStrLn . show . gc1)
			
checkGuard2 :: String -> CoALP ()
checkGuard2 q = whenProgram (
	\p -> case parseQuery q of
		Left err	-> iputStrLn err
		Right r		-> iputStrLn . show $ (gc2 r p)
	)
			
			
drawProgram :: CoALP ()
drawProgram = whenProgram (liftIO . displayProgram)

drawRew :: Int -> String -> CoALP ()
drawRew depth q = whenProgram (
	\p -> case parseQuery q of
		Left err	-> do
			iputStrLn err
			return ()
		Right r'	-> do
			iputStrLn $ "Query" ++ q ++ " loaded."
			let r = r'
			let rt = rew p r []
			liftIO . displayRewTree depth $ rt  --rew p r []
			--iputStrLn . show . (head 20) $ loops' rt
	)

drawTrans :: Int -> [Integer] -> String -> CoALP ()
drawTrans depth var q = whenProgram (
	\prog -> case parseQuery q of
		Left err	-> do
			iputStrLn err
			return ()
		Right r		-> do
			iputStrLn $ "Query " ++ q ++ " loaded."
			let rt = rew prog r []
			let tt = foldl (trans prog) rt (fmap mkVar var)
			--let tt = trans prog rt (mkVar $ head var)
			liftIO . displayRewTree depth $ tt 
			--iputStrLn . show . (head 20) $ loops' rt
	)

drawDer :: Int -> Int -> String -> CoALP ()
drawDer depD depR q = whenProgram (
	\prog -> case parseQuery q of
		Left err	-> do
			iputStrLn err
			return ()
		Right r		-> do
			iputStrLn $ "Query " ++ q ++ " loaded."
			--let rt = rew prog r []
			--let dt = undefined
			undefined
			--let tt = trans prog rt (mkVar $ head var)
			--liftIO . displayRewTree depth $ dt 
			--iputStrLn . show . (head 20) $ loops' rt
	)


verbPutStrLn :: String -> CoALP ()
verbPutStrLn str = do
	s <- get
	let verbosity = optVerbosity $ caOptions s
	when (verbosity >= VVerbose) $ iputStrLn str


whenProgram :: (Program1 -> CoALP ()) -> CoALP ()
whenProgram f = maybe (iputStrLn "No program loaded") f
	=<< program <$> get
		


