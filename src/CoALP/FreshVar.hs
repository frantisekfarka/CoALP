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
	, FreshVar
	, runFresh
	, evalFresh
	, getFresh
	, combine
) where

import Data.Bits (Bits, shiftL, setBit)

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


instance Monad (FreshVar v) where
	return a = FreshVar $ \v -> (a, v)
	m >>= k  = FreshVar $ \v -> 
		let (a, v') = runFresh m v 
		in runFresh (k a) v'

-- | Get next fresh variable
getFresh :: (Bits v, Num v) => FreshVar v v -- ^ TODO is Num for efficiency neccessary?
getFresh = FreshVar $ \v -> (v, v + 1)

-- | Combine two computations with fresh variables into
-- sinle while spliting the source
combine :: (Bits v) => 
	(FreshVar v a)	-- ^ The first computation
	-> (FreshVar v b)	-- ^ The second computation
	-> FreshVar v (a, b)
combine ma mb = FreshVar $ \v -> let
		(v', v'') = split v
		(va, vb) = split v''
		(a, _) = runFresh ma va
		(b, _) = runFresh mb vb
		in ((a,b), v')
	where
		split v = let v' = shiftL v 1 in (shiftL v' 1, shiftL (setBit v' 0) 1)

-- | Just a name for initial 0
initFresh :: Integer
initFresh = 0


testFresh :: FreshVar Integer String
testFresh = do
	v <- getFresh
	(s1, s2) <- subComp `combine` subComp
	v' <- getFresh
	return $ "The result of subcomps is " ++ s1 ++ " and " ++ s2 ++  
		" We also have vars " ++ showB v ++ " and " ++ showB v'


{-
subComp :: FreshVar Integer String
subComp = do
	v <- getFresh
	v' <- getFresh
	return $ "R (" ++ showB v ++ ", " ++ showB v' ++ ")"


showB a = showIntAtBase 2 intToDigit a ""
-}

