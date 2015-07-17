module CoALPj.Actions (
	  loadFile
	, reloadFile
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
        , convert
        , annotate
        , transform
	) where

import Control.Monad (when, liftM2)
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
        , programA
        , varCount 
	, optVerbosity
	, Verbosity (..)
	)

-- TODO refactor
import CoALP.Render (displayProgram,displayRewTree,displayDerTree, ppProgram)
import CoALP.Guards (gc1,gc2,gc3,gc3one,derToUnc,derToUng, getProgramLoops)
import CoALP.Program (Program1, ProgramA)
import CoALP.Parser.Parser (parse,parseWithCount,parseClause)
--import CoALP.Parser.PrettyPrint (ppProgram)
import CoALP.RewTree (rew)
import CoALP.DerTree (der,trans,Vr(..))

import CoALP.Transform (transformProg, transformProgA, annotateProg, annotateProgA, toProgramA)


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
	case parseWithCount cnt of
		Left err	-> do
			iputStrLn err
			return ()
		Right (prg, c) -> do
			s <- get
			put $ s { program = Just (reverse prg), programPath = Just file, varCount = Just c}
			when (optVerbosity (caOptions s) >= Default) (iputStrLn $ 
				"Program " ++ file ++ " loaded.")

reloadFile :: CoALP ()
reloadFile = do
	pp <- programPath <$> get
	dropProgram 
	(maybe (iputStrLn "No program loaded")) loadFile pp

dropProgram :: CoALP ()
dropProgram = (\s -> put $ s { program = Nothing, programPath = Nothing, programA = Nothing } ) =<< get

convert :: CoALP ()
convert = whenProgram (
          \prg -> do
                s <- get
                let prgA = toProgramA prg
                put $ s { programA = Just prgA }
                when (optVerbosity (caOptions s) >= Default) (iputStrLn $
                        "Program converted to Annotated Version.")
          )
			

transform :: CoALP ()
transform = whenProgACount (
            \pc -> do 
              s <- get
              let (t, c) = transformProgA pc
              putPrgAState (t,c)
              when (optVerbosity (caOptions s) >= Default) (iputStrLn $
                     "Program transformed."
                     ++ "\nOriginal\n"++ ppProgram (fst pc)
                     ++ "\nTransformed\n" ++ ppProgram t 
                     ++ "\n") 
            )

annotate :: CoALP ()
annotate = whenProgACount (
           \pc -> do
             s <- get
             let loops = getProgramLoops (fst pc)
                 (t,c) = transformProgA pc
                 anno  = annotateProgA t loops 
             putPrgAState (anno, c)
             when (optVerbosity (caOptions s) >= Default) (iputStrLn $
                     "Program annotated."
                     ++ "\nOriginal\n"++ ppProgram (fst pc) 
                     ++ "\nLoops found\n" ++ (show $ loops)
                     ++ "\nAnnotated\n" ++ ppProgram anno
                     ++ "\n") 
           )


putPrgState :: (Program1, Integer) -> CoALP ()
putPrgState (p,c) = do
        s <- get
        put $ s { program = Just p, varCount = Just c }

putPrgAState :: (ProgramA, Integer) -> CoALP()
putPrgAState (p,c) = do
        s <- get
        put $ s { programA = Just p, varCount = Just c }


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

whenPrgOrPrgA :: (Either Program1 ProgramA -> CoALP ()) -> CoALP()
whenPrgOrPrgA f = maybe (iputStrLn "No program converted ") f
        =<< prog <$> get
        where prog p = case programA p of
                         Just a -> Just (Right a)
                         Nothing -> case program p of
                                      Just a -> Just (Left a)
                                      Nothing -> Nothing

whenProgACount :: ((ProgramA, Integer) -> CoALP ()) -> CoALP ()
whenProgACount f = maybe (iputStrLn "No program converted ") f
        =<< progCount <$> get
        where progCount s = liftM2 (,) (programA s) (varCount s)


