{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
-- 
-- | This module provied FreshVar monad,
-- the Monad gives access to unlimited fresh vars through
-- `getFresh` operation. It also allows to split source of FreshVars
-- in between two monadic computations
--
-- in priciple it is a state monad that carries around and splits counter
--
module CoALP.FreshVar (
	  FreshVar (..)
	, evalFresh
	, getFresh
	, Freshable (..)
) where

import Control.Applicative (Applicative, pure, (<*>))
import Data.Bits (shiftL, shiftR, setBit)
import CoALP.Program(AnnoVar(..))

---------------------------------------------------------------------------
-- | /getFresh/ returns fresh variable from the internals of the monad.
--
-- /combine/ merges two computations with fresh variables

---------------------------------------------------------------------------
-- | A parameterizable fresh var monad where /v/ is the type of the state
-- to carry and /a/ is the type of the /return value/.
newtype FreshVar v a = FreshVar { runFresh :: v -> (a, v) }

-- | Evaluate this monad with the given initial counter, throwing
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
getFresh = FreshVar $ \v -> split $ v

-- | Class to force initial 0 to be polymorphic
class Freshable a where
	initFresh :: a -- Consider removing for cases of initialising CoInd vs Ind.
	split :: a -> (a,a)
	apartL :: a -> a
	apartR :: a -> a
	unpart :: a -> a

instance Freshable Integer where
	initFresh = 1
	split v = let v' = shiftL v 1 in (v', setBit v' 0)
	apartL v = shiftL v 1
	apartR v = setBit (shiftL v 1) 0
	unpart v = shiftR v 1

instance Freshable (AnnoVar Integer)  where
        initFresh = Ind 1
	split (Ind v) = let v' = shiftL v 1 in (Ind v', Ind (setBit v' 0))
	split (CoInd v) = let v' = shiftL v 1 in (CoInd v', CoInd (setBit v' 0))
	apartL (Ind v) = Ind (shiftL v 1)
        apartL (CoInd v) = CoInd (shiftL v 1)
	apartR (Ind v) = Ind (setBit (shiftL v 1) 0)
	apartR (CoInd v) = CoInd (setBit (shiftL v 1) 0)
	unpart (Ind v) = Ind (shiftR v 1)
	unpart (CoInd v) = CoInd (shiftR v 1)


