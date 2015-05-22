{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
-- 
-- | This module provied FreshVar monad
--
-- the Monad gives access to unlimited fresh vars through
-- `getFresh` operation. It also allows to split source of FreshVars
-- in between two monadic computations
--
-- in priciple it is a state monad that carries around and splits counter
--
module CoALP.FreshVar (
	  initFresh
	, apartL
	, apartR
	, FreshVar
	, runFresh
	, evalFresh
	, getFresh
	, Freshable
) where

import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (foldM)
import Data.Bits (Bits, shiftL, setBit)


import Numeric (showIntAtBase)

showBin b = interspace $ replicate pad '0' ++ bin
	where
		bin = showIntAtBase 2 (head.show) b ""
		pad = 4 - ((length bin) `mod` 4)
		interspace [] = []
		interspace xs = take 4 xs ++ " " ++ (interspace $ drop 4 xs)


---------------------------------------------------------------------------
-- | /getFresh/ returns fresh variable from the internals of the monad.
--
-- /combine/ merges two computations with fresh variables

--class (Monad m) => MonadState s m | m -> s where
--	get :: m s
--	put :: s -> m ()

---------------------------------------------------------------------------
-- | A parameterizable fresh var monad where /v/ is the type of the state
-- to carry and /a/ is the type of the /return value/.

newtype FreshVar v a = FreshVar { runFresh :: v -> (a, v) }

-- |Evaluate this monad with the given initial counter, throwing
-- away the final counter.  Very much like @fst@ composed with
-- @runFreshVar@.
--
evalFresh :: FreshVar v a -- ^The fresh var to evaluate
	-> v	-- ^An initial value of counter
	-> a	-- ^The return value of the computation
evalFresh m x = fst (runFresh m x)

instance Functor (FreshVar v) where
	fmap f a = FreshVar $ \v -> 
		let (a', v') = runFresh a v
		in (f a', v')


instance (Freshable v) => Applicative (FreshVar v) where
	pure a = FreshVar $ \v -> (a, v)
	f <*> a = FreshVar $ \v -> let 
			v' = v
			(vf, va) = split v --''
			(pf, _) = runFresh f vf
			(pa, _) = runFresh a va
		in (pf pa, v')
	{-f <*> a = FreshVar $ \v -> let 
			(pf, v') = runFresh f v
			(pa, v'') = runFresh a v'
		in (pf pa, v'')
	-}


instance (Freshable v) => Monad (FreshVar v) where
	return a = FreshVar $ \v -> (a, v)
	m >>= k  = FreshVar $ \v -> 
		let (a, v') = runFresh m v 
		in runFresh (k a) v'

-- | Get next fresh variable
getFresh :: (Freshable v) => FreshVar v v -- ^ TODO is Num for efficiency neccessary?
--getFresh = FreshVar $ \v -> (v, v + 1)
getFresh = FreshVar $ \v -> split $ v

-- | Class to force initial 0 to be polymorphic
class Freshable a where
	initFresh :: a
	split :: a -> (a,a)
	apartL :: a -> a
	apartR :: a -> a

instance Freshable Integer where
	initFresh = 1
	--split v = let v' = shiftL v 1 in (shiftL v' 1, shiftL (setBit v' 0) 1)
	split v = let v' = shiftL v 1 in (v', setBit v' 0)
	apartL v = shiftL v 1
	apartR v = setBit (shiftL v 1) 0



{-
testFresh :: FreshVar Integer String
testFresh = do
	v <- getFresh
	(s1, s2) <- subComp `combine` subComp
	v' <- getFresh
	return $ "The result of subcomps is " ++ s1 ++ " and " ++ s2 ++  
		" We also have vars " ++ showB v ++ " and " ++ showB v'


subComp :: FreshVar Integer String
subComp = do
	v <- getFresh
	v' <- getFresh
	return $ "R (" ++ showB v ++ ", " ++ showB v' ++ ")"


showB a = showIntAtBase 2 intToDigit a ""
-}

