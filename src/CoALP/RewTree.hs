-- | Module thtat constructs rewriting tree
module CoALP.RewTree (
	  rew
	, trans
	, mkVar
) where

import Control.Arrow (first)

import Data.Functor ((<$>))
import Data.Traversable (sequenceA)

import CoALP.FreshVar (FreshVar,getFresh,evalFresh,Freshable,initFresh)
import CoALP.Program (Program, Clause(..), Subst, RewTree(..),
	AndNode(..),OrNode(..),Term(..),Query(..),Vr(..),mkVar)

-- TODO rew :: {-(Show a, Show b, Show c) => -} d ~ Integer => Program a b c -> Query a b c -> Subst a b c -> RewTree a b c d
rew :: Freshable d => Program a b c -> Query a b c -> Subst a b c -> RewTree a b c d
rew p c@(Query b) s = flip evalFresh initFresh $ do
	ands <- sequenceA $ fmap (mkAndNode p) b
	return $ RT c s ands

--makeAnds :: Program a b c -> Term a b c -> [AndNode (Clause a b c)]

-- | AndNode aka Term node
mkAndNode :: (Freshable d) => Program a b c -> Term a b c -> FreshVar d (AndNode (Clause a b c) (Term a b c) (Vr d))
mkAndNode p t = do
	ors <- sequenceA $ fmap (mkOrNode p t) p
	return $ AndNode t ors

mkOrNode :: (Freshable d) => Program a b c -> Term a b c -> Clause a b c -> FreshVar d (OrNode (Clause a b c) (Term a b c) (Vr d))
mkOrNode p t (Clause h b)  = case h `match` t of
	Just s	->	let sb = (s `subst`) <$> b
			in do
				ands <- sequenceA ((mkAndNode p) <$> sb)
				return $ OrNode (Clause t sb) ands
	--Just s	->	OrNode (Clause h (([] `subst`) <$> b)) (fmap (mkAndNode p) b)
	Nothing	->	getFresh >>= return . OrNodeEmpty . Vr


-- TODO proper matching¡
match :: {-(Show a, Show b, Show c) =>-} Term a b c -> Term a b c -> Maybe (Subst a b c)
match (Var x1) 		(Var x2)	= Just $ (x1, Var x2):[] -- ? should be fresh?
match (Fun id1 t1)	(Fun id2 t2)	= if id1 == id2
	then concat <$> sequence (zipWith match t1 t2) -- ^ TODO it is neccessary to combine
	-- the partial mathces properly, this is buggy
	else Nothing
-- Just $ id1 == id2 && (all (== True) $ zipWith match t1 t2)
--match (Fun _ []) 	(Var _)		= Just True
match (Var x) 		(Fun id1 ts)	= Just $ (x, Fun id1 ts):[]
match _ 		_		= Nothing



-- | apply substitution to the term tree
subst :: Subst a b c -> Term a b c -> Term a b c
subst s (Var x)		= maybe (Var x) id (x `lookup` s)
subst s (Fun id ts)	= Fun id (subst s <$> ts)

-- | compute the rew tree transition
-- TODO make sure this works for infinite tree
trans :: (Eq d, Show d, Integral d) => Program a b c -> RewTree a b c d -> Vr d ->  RewTree a b c d
trans p origT vr = findVar origT
	where
		findVar (RT _ _ ands) = error $ "Here: " ++ show (filter ((== vr).fst) $
			concatMap processAnd ands)
		
		processAnd :: AndNode a b c -> [(c,Int)]
		processAnd (AndNode _ ors) = (concat $ zipWith processOr ors [0..]) ++ 
			concatMap continueOr ors
		processOr (OrNode _ _) _ = []
		processOr (OrNodeEmpty d) c = [(d, c)]
		continueOr (OrNode _ ands) = concatMap processAnd ands
		continueOr _ = []




