module CoALPj.Actions (
	  loadFile
	, reloadFile
        , transformFile
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
        , annotateFile
	) where

import Control.Monad (when)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.State (get, put)

import Data.Functor ((<$>))

import System.IO.Error (tryIOError)

import CoALP.Error (Err(..))
import CoALPj.InternalState (
	  CoALP
	, caOptions
	, program
	, programPath
	, optVerbosity
	, Verbosity (..)
	)

-- TODO refactor
import CoALP.Render (displayProgram,displayRewTree,displayDerTree, ppProgram)
import CoALP.Guards (gc1,gc2,gc3,gc3one,derToUnc,derToUng, getProgramLoops)
import CoALP.Program (Program1)
import CoALP.Parser.Parser (parse,parseWithCount,parseClause)
--import CoALP.Parser.PrettyPrint (ppProgram)
import CoALP.RewTree (rew)
import CoALP.DerTree (der,trans,Vr(..))

import CoALP.Transform (transformProg, annotateProg)


-- TODO repeats in REPL.hs, merge to helper module
iputStrLn :: String -> CoALP ()
iputStrLn s = runIO $ putStrLn s

-- | A version of liftIO that puts errors into the error type of the CoALPj monad
-- TODO is the use of ExceptT neccessary?
-- DITTO merge
runIO :: IO a -> CoALP a
runIO x = lift $ liftIO (tryIOError x) >>= (either (throwE . Msg . show) return)

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
			s <- get
			put $ s { program = Just (reverse prg), programPath = Just file }
			when (optVerbosity (caOptions s) >= Default) (iputStrLn $ 
				"Program " ++ file ++ " loaded.")

reloadFile :: CoALP ()
reloadFile = do
	pp <- programPath <$> get
	dropProgram 
	(maybe (iputStrLn "No program loaded")) loadFile pp

transformFile :: FilePath -> CoALP ()
transformFile file = do
        cnt <- lift . lift $ readFile file
        case parseWithCount cnt of
                Left err         -> do
                        iputStrLn err
                        return ()
                Right (prg, count) -> do
                        s <- get
                        put $ s { program = Just transformed, programPath = Just file }
			when (optVerbosity (caOptions s) >= Default) (iputStrLn $ 
				"Program " ++ file ++ " loaded and transformed.")
                        where transformed = transformProg (reverse prg, count+1)

annotateFile :: FilePath -> CoALP ()
annotateFile file = do
        cnt <- lift . lift $ readFile file
        case parseWithCount cnt of
                Left err         -> do
                        iputStrLn err
                        return ()
                Right (prg, count) -> do
                        s <- get
                        put $ s { program = Just annotated, programPath = Just file }
			when (optVerbosity (caOptions s) >= Default) (iputStrLn $ 
				"Program " ++ file ++ " loaded, transformed and annotated."
                                ++ "\nOriginal\n"++ ppProgram p 
                                ++ "\nTransformed:\n" ++ ppProgram transformed 
                                ++ "\nAnnotated:\n" ++ ppProgram annotated
                                ++ "\nLoops: " ++ (show $ getProgramLoops p) ++ "\n")
                        where p = reverse prg
                              transformed = transformProg (p, count+1)
                              annotated = annotateProg transformed (getProgramLoops p)		
--realTrans = whenProgram (\p -> iputStrLn . ppProgram . annotate p $ getProgramLoops p)

dropProgram :: CoALP ()
dropProgram = (\s -> put $ s { program = Nothing, programPath = Nothing } ) =<< get
			
-- | print program
printProgram :: CoALP ()
printProgram = whenProgram (iputStrLn . ppProgram)
		
checkGuard1 :: CoALP ()
checkGuard1 = whenProgram (iputStrLn . show . gc1)
			
checkGuard2 :: String -> CoALP ()
checkGuard2 c = whenProgram (
	\p -> case parseClause c of
		Left err	-> iputStrLn err
		Right r		-> iputStrLn . show $ (gc2 p r)
	)
			
checkGuard3 :: CoALP ()
checkGuard3 = whenProgram (\p -> iputStrLn . show $ (gc3 p))
			
checkGuard3One :: String -> CoALP ()
checkGuard3One c = whenProgram (
	\p -> case parseClause c of
		Left err	-> iputStrLn err
		Right r		-> iputStrLn . show $ (gc3one p r)
	)
	
			
drawProgram :: CoALP ()
drawProgram = whenProgram (liftIO . displayProgram)

drawRew :: Int -> String -> CoALP ()
drawRew depth q = whenProgram (
	\p -> case parseClause q of
		Left err	-> do
			iputStrLn err
			return ()
		Right r'	-> do
			iputStrLn $ "Query " ++ q ++ " loaded."
			let r = r'
			let rt = rew p r []
			liftIO . displayRewTree depth $ rt  --rew p r []
			--iputStrLn . show . (head 20) $ loops' rt
	)

drawTrans :: Int -> [Integer] -> String -> CoALP ()
drawTrans depth var q = whenProgram (
	\prog -> case parseClause q of
		Left err	-> do
			iputStrLn err
			return ()
		Right r		-> do
			iputStrLn $ "Query " ++ q ++ " loaded."
			let rt = rew prog r []
			let tt = fst $ foldl (trans prog . fst) (rt, Nothing) (fmap Vr var)
			liftIO . displayRewTree depth $ tt 
			--iputStrLn . show . (head 20) $ loops' rt
	)

drawDer :: Int -> Int -> String -> CoALP ()
drawDer depD depR q = whenProgram (
	\prog -> case parseClause q of
		Left err	-> do
			iputStrLn err
			return ()
		Right r		-> do
			iputStrLn $ "Query " ++ q ++ " loaded."
			liftIO . displayDerTree depD depR $ der prog r 
			--iputStrLn . show . (head 20) $ loops' rt
	)

drawInf :: Int -> Int -> String -> CoALP ()
drawInf depD depR q = whenProgram (
	\prog -> case parseClause q of
		Left err	-> do
			iputStrLn err
			return ()
		Right r		-> do
			iputStrLn $ "Query " ++ q ++ " loaded."
			liftIO . displayDerTree depD depR $ derToUnc depD $ der prog r 
			--iputStrLn . show . (head 20) $ loops' rt
	)

drawUng :: Int -> Int -> String -> CoALP ()
drawUng depD depR q = whenProgram (
	\prog -> case parseClause q of
		Left err	-> do
			iputStrLn err
			return ()
		Right r		-> do
			iputStrLn $ "Query " ++ q ++ " loaded."
			liftIO . displayDerTree depD depR $ derToUng depD $ der prog r 
			--iputStrLn . show . (head 20) $ loops' rt
	)

{-verbPutStrLn :: String -> CoALP ()
verbPutStrLn str = do
	s <- get
	let verbosity = optVerbosity $ caOptions s
	when (verbosity >= VVerbose) $ iputStrLn str
-}

whenProgram :: (Program1 -> CoALP ()) -> CoALP ()
whenProgram f = maybe (iputStrLn "No program loaded") f
	=<< program <$> get
		


