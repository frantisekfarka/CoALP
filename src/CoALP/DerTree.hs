-- | Module that constructs rewriting tree
module CoALP.DerTree (
	  der
	, trans
	, mkVar
) where

import CoALP.RewTree (rew, getVrs)
import CoALP.FreshVar (FreshVar,getFresh,evalFresh,Freshable,initFresh)
import CoALP.Unify (match, unify, applySubst, composeSubst)
import CoALP.Program (Program, Clause(..), Subst, RewTree(..), DerTree(..),
	AndNode(..),OrNode(..),Term(..),Query(..),Vr(..),mkVar, Trans(..)
	)

-- | compute the rew tree transition
-- TODO make sure this works for infinite tree
--trans :: (Eq a, Eq b, Ord b, Eq d, Show d, Integral d, Show c, Show b, Show a, Freshable b, Freshable d)
--	=> Program a b c -> RewTree a b c d -> Vr d ->  RewTree a b c d
--trans :: (Eq a, Eq b, Ord b, Eq d, Freshable b, Freshable d)
--	=> Program a b c -> RewTree a b c d -> Vr d ->  RewTree a b c d
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




der :: (Eq a, Eq b, Ord b, Freshable b, Freshable d, Show c, Show b, Show a, Eq d) =>
	Program a b c -> Query a b c -> DerTree a b c d
der p q = derT p $ rew p q []

derT :: (Eq a, Eq b, Ord b, Freshable b, Freshable d, Show c, Show b, Show a, Eq d) =>
	Program a b c -> RewTree a b c d -> DerTree a b c d
derT p rt = DT rt (fmap (\v -> Trans v $ derT p $ trans p rt (Vr v)) (getVrs rt))



