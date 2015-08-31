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
	, drawUnsafe
	, drawInf
	, drawUng
	, resolve
	, nextRes
        , convert
        , annotate
        , transform
        , antiUnify
	, printSig
	) where

import Control.Arrow
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
	, resolves
        , programA
        , varCount 
	, signature
	, optVerbosity
	, Verbosity (..)
	)

-- TODO refactor
import CoALP.Render (
	  displayProgram
	, displayRewTree
	, displayDerTree
	, displayObsTree
	, displayDerTreeUnsafe
	, ppClause
	, ppTerm
	, ppProgram
	)
import CoALP.Render (displayProgram,displayRewTree,displayDerTree, ppProgram, ppTerm, ppSubst)
import CoALP.Guards (gc1,gc2,gc3,gc3one,derToUnc,derToUng, getProgramLoops)

import CoALP.Program (Program1, Signature1, ProgramA, RewTree1, RewTreeA, Succ(..), Vr(..),Type(..),lookupType, Ident)

import CoALP.Parser.Parser (parseWithCount,parseClause)
import CoALP.RewTree (rew)
import CoALP.DerTree (der,trans)
import CoALP.Sound (res)

import CoALP.Transform (transformProgA, annotateProgA, toProgramA, toClauseA)
import CoALP.AntiUnify (antiUnifyClause)

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
		Right ((prg, sig), c) -> do
			s <- get
			put $ s { program = Just (reverse prg)
				, programA = Nothing
				, programPath = Just file
				, varCount = Just c
				, signature = Just sig 
				}
			when (optVerbosity (caOptions s) >= Default) (iputStrLn $ 
				"Program " ++ file ++ " loaded.")

reloadFile :: CoALP ()
reloadFile = do
	pp <- programPath <$> get
	dropProgram 
	(maybe (iputStrLn "No program loaded")) loadFile pp

dropProgram :: CoALP ()
dropProgram = (\s -> put $ s {
		  program = Nothing
		, programA = Nothing
		, varCount = Nothing
		, programPath = Nothing
		, resolves = Nothing
		} ) =<< get

-- | Convert a program from Program1 to ProgramA
convert :: CoALP ()
convert = whenProgram (
          \p _ -> do
                s <- get
                let prgA = toProgramA p
                put $ s { programA = Just prgA }
                when (optVerbosity (caOptions s) >= Default) (iputStrLn $
                        "Program converted to Annotatetable Version.")
          )
			
-- | Apply RT to ProgramA
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

-- | Apply RT* to ProgramA
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
                     ++ "\nAnnotated\n" ++ ppProgram anno
                     ++ "\n") 
           )

-- | Update ProgramA and Count in the state
putPrgAState :: (ProgramA, Integer) -> CoALP()
putPrgAState (p,c) = do
        s <- get
        put $ s { programA = Just p, varCount = Just c }

antiUnify :: String -> CoALP()
antiUnify c = case parseClause c of
                Left err -> iputStrLn err
                Right r  -> case antiUnifyClause r of
                              Left er -> iputStrLn er
                              Right t -> iputStrLn $
                                           "AntiUnifier: " ++ ppTerm (fst t)
                                           ++ "\nSubstitutions: " ++ ppSubst subs
                                           where subs = map (\((t1, _), v) -> (v, t1)) (snd t)
 
-- | print program
printProgram :: CoALP ()
printProgram = whenPrgOrPrgA (eitherM (iputStrLn . ppProgram) (iputStrLn . ppProgram))

checkGuard1 :: CoALP ()
checkGuard1 =  whenPrgOrPrgA (eitherM (iputStrLn . show . gc1) (iputStrLn . show . gc1))
			
checkGuard2 :: String -> CoALP ()
checkGuard2 c = whenPrgOrPrgA (
          \p -> case parseClause c of
                  Left err -> iputStrLn err
                  Right r  -> eitherM (iputStrLn . show . flip gc2 r) (iputStrLn . show . flip gc2 (toClauseA r)) p
          )

		
checkGuard3 :: CoALP ()
checkGuard3 = whenPrgOrPrgA (eitherM (iputStrLn . show . gc3) (iputStrLn . show . gc3))
			
resolve :: String -> CoALP ()
resolve c = whenProgram (
	\p sig -> case parseClause c of
		Left err	-> iputStrLn err
		Right r		-> do
			s <- get
			put $ s { resolves = Just $ res p sig r } 
			-- iputStrLn . show $ (res p r)
			nextRes
	)

nextRes :: CoALP ()
nextRes = do
	res <- fmap (resolves) $ get
	case res of
		Nothing	-> iputStrLn "no query yet"
		Just [] -> iputStrLn "False"
		Just (h:ts) -> do	
			iputStrLn (d h)
			s <- get
			put $ s { resolves = Just $ dropWhile (== h) ts }
	where
		d (IndS c) = "True\n\tobserved inductively: " ++ ppClause c
		d (CoIndS c gc) = "True\n\tobserved co-inductively: " ++ ppClause c ++
			"\n\t  GC: " ++ concatMap g gc
		g ((ix, t, v)) = "( " ++ show ix ++ ", " ++ ppTerm t ++ ", " ++ show v ++ "),"

			
checkGuard3One c = whenPrgOrPrgA (
         \p -> case parseClause c of
                 Left err -> iputStrLn err
                 Right r  -> eitherM (iputStrLn . show . flip gc3one r) (iputStrLn . show . flip gc3one (toClauseA r)) p
         )
			
			
drawProgram :: CoALP ()
drawProgram = whenPrgOrPrgA (eitherM (liftIO . displayProgram) (liftIO . displayProgram))

drawRew :: Int -> String -> CoALP ()
drawRew depth q = whenPrgOrPrgA (
        \p -> case parseClause q of
                Left err  -> do
                        iputStrLn err
			return ()
                Right r'  -> do
                        iputStrLn $ "Query " ++ q ++ " loaded."
			let r = r'
                        eitherM (liftIO . displayRewTree depth . (rt r)) (liftIO . displayRewTree depth . (rt' r)) p 
			--iputStrLn . show . (head 20) $ loops' rt
                        where rt  res pr = rew pr res [] :: RewTree1
                              rt' res pr = rew pr (toClauseA res) [] :: RewTreeA
        )


drawTrans :: Int -> [Integer] -> String -> CoALP ()
drawTrans depth var q = whenPrgOrPrgA (
        \prog -> case parseClause q of
		Left err	-> do
			iputStrLn err
			return ()
		Right r		-> do
			iputStrLn $ "Query " ++ q ++ " loaded."
			eitherM (liftIO . displayRewTree depth . tt) (liftIO . displayRewTree depth . tt') prog
			--iputStrLn . show . (head 20) $ loops' rt
                        where rt  res pr = rew pr res []
                              --rt' res pr = rew pr (toClauseA res) [] :: RewTreeA 
			      tt  pr = fst $ foldl (\x y -> trans pr (fst x) y []) (rt  r pr, Nothing) (fmap Vr var)
			      tt' pr = fst $ foldl (\x y -> trans pr (fst x) y []) (rt (toClauseA r) pr, Nothing) (fmap Vr var)
	)

drawDer :: Int -> Int -> String -> CoALP ()
drawDer depD depR q = whenPrgOrPrgA (
	\prog -> case parseClause q of
		Left err	-> do
			iputStrLn err
			return ()
		Right r		-> do
			iputStrLn $ "Query " ++ q ++ " loaded."
			eitherM (liftIO . displayDerTree depD depR . (flip der r)) (liftIO . displayDerTree depD depR . (flip der (toClauseA r))) prog
			--iputStrLn . show . (head 20) $ loops' rt
	)

drawUnsafe :: Int -> Int -> String -> CoALP ()
drawUnsafe depD depR q = whenProgram (
	\p _ -> case parseClause q of
		Left err	-> do
			iputStrLn err
			return ()
		Right r		-> do
			iputStrLn $ "Query " ++ q ++ " loaded."
			liftIO . displayDerTreeUnsafe depD depR $ der p r 
			--iputStrLn . show . (head 20) $ loops' rt
	)


drawInf :: Int -> Int -> String -> CoALP ()
drawInf depD depR q = whenPrgOrPrgA (
	\prog -> case parseClause q of
		Left err	-> do
			iputStrLn err
			return ()
		Right r		-> do
			iputStrLn $ "Query " ++ q ++ " loaded."
			eitherM (liftIO . displayDerTree depD depR . (derToUnc depD . dt)) (liftIO . displayDerTree depD depR . (derToUnc depD . dt')) prog 
			where dt  = flip der r
                              dt' = flip der (toClauseA r)
                        --iputStrLn . show . (head 20) $ loops' rt
	)

drawUng :: Int -> Int -> String -> CoALP ()
drawUng depD depR q = whenPrgOrPrgA (
	\prog -> case parseClause q of
		Left err	-> do
			iputStrLn err
			return ()
		Right r		-> do
			iputStrLn $ "Query " ++ q ++ " loaded."
			eitherM (liftIO . displayDerTree depD depR . (derToUng depD . dt)) (liftIO . displayDerTree depD depR . (derToUng depD . dt')) prog
                        where dt  = flip der r
                              dt' = flip der (toClauseA r)
			--iputStrLn . show . (head 20) $ loops' rt
	)

printSig :: Ident -> CoALP ()
printSig ident = maybe (iputStrLn "No program loaded") (
			\sig' -> case lookupType sig' ident of
				SInd	-> iputStrLn $ "inductive : " ++ ident
				SCoInd	-> iputStrLn $ "coinductive : " ++ ident
			) =<< signature <$> get 


whenProgram :: (Program1 -> Signature1 -> CoALP ()) -> CoALP ()
whenProgram f = maybe (iputStrLn "No program loaded") (uncurry f)
	=<<  (g . (program &&& signature)) <$> get
	where
		g (Just a, Just b) = Just (a, b)
		g _ = Nothing

whenPrgOrPrgA :: (Either Program1 ProgramA -> CoALP ()) -> CoALP()
whenPrgOrPrgA f = maybe (iputStrLn "No program loaded ") f
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

eitherM :: Monad m => (a -> m c) -> (b -> m c) -> Either a b -> m c
eitherM f _ (Left x)  = f x
eitherM _ f (Right x) = f x
