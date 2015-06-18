{-# LANGUAGE FlexibleContexts #-}
-- | Module thtat constructs rewriting tree
module CoALP.RewTree (
	  rew
	, trans
	, mkVar
) where

import Data.Functor ((<$>))
import Data.Traversable (sequenceA)

import CoALP.FreshVar (FreshVar,getFresh,evalFresh,Freshable,initFresh)
import CoALP.Unify (match, unify, applySubst, composeSubst)
import CoALP.Program (Program, Clause(..), Subst, RewTree(..),
	AndNode(..),OrNode(..),Term(..),Query(..),Vr(..),mkVar
	)

import Debug.Trace

-- TODO rew :: {-(Show a, Show b, Show c) => -} d ~ Integer => Program a b c -> Query a b c -> Subst a b c -> RewTree a b c d
rew :: (Eq a, Eq b, Ord b, Freshable d) =>
	Program a b c -> Query a b c -> Subst a b c -> RewTree a b c d
rew p c@(Query b) s = flip evalFresh initFresh $ do
		ands <- sequenceA $ fmap (mkAndNode p s) b'
		return $ RT c s ands
	where
		b' = fmap (applySubst s) b

--makeAnds :: Program a b c -> Term a b c -> [AndNode (Clause a b c)]

-- | AndNode aka Term node
mkAndNode :: (Eq a, Eq b, Ord b, Freshable d) => 
	Program a b c -> Subst a b c -> Term a b c -> FreshVar d (AndNode (Clause a b c) (Term a b c) (Vr d))
mkAndNode p os t = do
	ors <- sequenceA $ fmap (mkOrNode p os t) p
	return $ AndNode t ors

mkOrNode :: (Eq a, Eq b, Ord b, Freshable d) =>
	Program a b c -> Subst a b c -> Term a b c -> Clause a b c -> FreshVar d (OrNode (Clause a b c) (Term a b c) (Vr d))
mkOrNode p os t (Clause h b)  = case h `match` t of
	Just s	->	let sb = ((os `composeSubst` s) `subst`) <$> b
			in do
				ands <- sequenceA ((mkAndNode p os) <$> sb)
				return $ OrNode (Clause t sb) ands
	--Just s	->	OrNode (Clause h (([] `subst`) <$> b)) (fmap (mkAndNode p) b)
	Nothing	->	getFresh >>= return . OrNodeEmpty . Vr




-- | apply substitution to the term tree
subst :: Subst a b c -> Term a b c -> Term a b c
subst s (Var x)		= maybe (Var x) id (x `lookup` s)
subst s (Fun idnt ts)	= Fun idnt (subst s <$> ts)

-- | compute the rew tree transition
-- TODO make sure this works for infinite tree
trans :: (Eq a, Eq b, Ord b, Eq d, Show d, Integral d, Show c, Show b, Show a, Freshable b, Freshable d)
	=> Program a b c -> RewTree a b c d -> Vr d ->  RewTree a b c d
trans _ RTEmpty _ = RTEmpty
trans p (origT@(RT q s ands)) vr = case ms' of
		Just s'	-> rew p q (s `composeSubst` s')
		Nothing 	-> RTEmpty
	where
		ms' = unify term (cHead (p !! pIx))
		(var, pIx, term):_ = (filter ((== vr).fst') $
			concatMap processAnd ands)
		
		processAnd :: AndNode e (Term a b c) (Vr d) -> [(Vr d,Int,Term a b c)]
		processAnd (AndNode t ors) = (concat $ zipWith (processOr t) ors [0..]) ++ 
			concatMap continueOr ors

		processOr :: Term a b c -> OrNode e (Term a b c) (Vr d) -> Int -> [(Vr d, Int, Term a b c)]
		processOr _ (OrNode _ _) _ = []
		processOr t (OrNodeEmpty d) c = [(d, c, t)]

		continueOr (OrNode _ ands) = concatMap processAnd ands
		continueOr _ = []
		fst' (a,_,_) = a

		{-f :: (Freshable d, Show (RewTree a b c d), Show a1) =>
		     [(a1, Int, Term a b c)] -> [Char]
		f ((var, pIx, term):_) = "\nVariable " ++ show var ++ ",\n" ++
			"Parent term " ++ show term ++ ",\n" ++
			"Unify with the head of " ++ show (p !! pIx) ++ "\n" ++
			"Unification " ++ show (unify term (cHead (p !! pIx))) ++
			"New rew tree" ++ show (transT p origT (unify term (cHead (p !! pIx))))
		f [] = "Not found"
		-}
		cHead (Clause h _) = h





