-- | Module that constructs rewriting tree
module CoALP.DerTree (
	  der
	, trans
	, mkVar
	, clauseProj
) where

import Data.Maybe (maybeToList)

import CoALP.RewTree (rew, getVrs)
import CoALP.FreshVar (Freshable)
import CoALP.Unify (unify, applySubst, composeSubst, match)
import CoALP.Program (Program, Clause(..), Subst, RewTree(..), DerTree(..),
	AndNode(..),OrNode(..),Term(..),Vr(..),mkVar, Trans(..),
	GuardingContext
	)
import CoALP.Reductions (isVarReductOf,nvPropSub)

-- | compute the rew tree transition
-- TODO make sure this works for infinite tree
trans :: (Eq a, Eq b, Ord b, Eq d, Freshable b, Freshable d)
	=> Program a b c -> RewTree a b c d -> Vr d ->
	(RewTree a b c d, Maybe (Int, Subst a b c, Term a b c))
trans _ RTEmpty _ = (RTEmpty, Nothing)
trans p (RT cl s ands) vr = case ms' of
		Just s'	-> (rew p cl (s `composeSubst` s'), Just (pIx, s', term)) 
		Nothing 	-> (RTEmpty, Nothing)
	where
		ms' = unify term (cHead (p !! pIx))
		(_var, pIx, term):_ = (filter ((== vr).fst') $
			concatMap processAnd ands)
		
		processAnd :: AndNode e (Term a b c) (Vr d) -> [(Vr d,Int,Term a b c)]
		processAnd (AndNode t ors) = (concat $ zipWith (processOr t) ors [0..]) ++ 
			concatMap continueOr ors

		processOr :: Term a b c -> OrNode e (Term a b c) (Vr d) -> Int -> [(Vr d, Int, Term a b c)]
		processOr _ (OrNode _ _) _ = []
		processOr t (OrNodeEmpty d) c = [(d, c, t)]

		continueOr (OrNode _ ands') = concatMap processAnd ands'
		continueOr _ = []

		fst' (a,_,_) = a
		cHead (Clause h _) = h




der :: (Eq a, Eq b, Eq d, Ord b, Freshable b, Freshable d) =>
	Program a b c -> Clause a b c -> DerTree a b c d
der p c = (derT p $ rew p c [])

derT :: (Eq a, Eq b, Eq d, Ord b, Freshable b, Freshable d) =>
	Program a b c -> RewTree a b c d -> DerTree a b c d
derT p rt = DT rt $ fmap toTrans (getVrs rt)
	where
		toTrans v = let (rt', cp) = trans p rt v in Trans p v cp  $ derT p rt' -- $ derT p rt'

{-
clauseProj p gc@Nothing = trace "GC: no guarding context" Nothing
clauseProj p gc@(Just (ix, s, t)) = trace ("GC: \n" ++
		"\thead(P(k)) = " ++ show headC ++ "\n" ++
		"\tt = " ++ show t ++ "\n" ++
		"\tsigma = " ++ show s ++ "\n" ++
		"\tsigma(t) = " ++ show st ++ "\n" ++
		"\tt'' -- t is variable reduct of sigma(t): " ++ show isVR ++ "\n" ++
		"\tt's -- nonvar prop subterms: " ++ show subs ++ "\n" ++
		"\tmatchers: " ++ show (fmap (\u -> map ((match u) . fst)  subs) isVR) ++ "\n" ++
		"\tRES: " ++ show (res p gc)
	) gc
	where
		Clause headC _ = p !! ix
		isVR = t `isVarReductOf` st
		st = s `applySubst` t
		subs = nvPropSub headC
-}
clauseProj :: (Eq a, Ord b) => 
	Program a b c -> Maybe (Int, Subst a b c, Term a b c) -> GuardingContext a b c
clauseProj _ Nothing 		= []
clauseProj p (Just (ix, s, t))
	| Just t'' <- t `isVarReductOf` (s `applySubst` t),
	  Clause h _ <- p !! ix = do
			(t', v) <- nvPropSub h 
			_ <- maybeToList $ match t' t'' 
			return (ix, t', v) 
	| otherwise	= []





