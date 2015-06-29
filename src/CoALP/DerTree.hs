-- | Module that constructs rewriting tree
module CoALP.DerTree (
	  der
	, trans
	, mkVar
	, clauseProj
) where

import Data.List (nub)
import Data.Maybe (maybeToList)

import CoALP.RewTree (rew, getVrs,loops)
import CoALP.FreshVar (Freshable, apartR, apartL)
import CoALP.Unify (unify, applySubst, composeSubst, match)
import CoALP.Program (Program, Clause(..), Subst, RewTree(..), DerTree(..),
	AndNode(..),OrNode(..),Term(..),Vr(..),mkVar, Trans(..),
	GuardingContext, mapClause, mapTerm, mapSubst
	)
import CoALP.Reductions (isVarReductOf,nvPropSub)


import Debug.Trace

-- | compute the rew tree transition
-- TODO make sure this works for infinite tree
--trans :: (Eq a, Eq b, Ord b, Eq d, Freshable b, Freshable d)
--	=> Program a b c -> RewTree a b c d -> Vr d ->
--	(RewTree a b c d, Maybe (Int, Subst a b c, Term a b c))
trans _ RTEmpty _ = (RTEmpty, Nothing)
trans p' rt@(RT cl' osi' ands') vr = case term `unify` h of
		Just si	-> (mkRew si, Just (pi, si', term)) 
		Nothing -> (RTEmpty, Nothing)
	where
		mkRew th = rew p (th `lap` cl) (th `composeSubst` si')
		(_, term', pi):_ = filter ((== vr).fst') $ getVrs rt
		fst' (a,_,_) = a
		lap th (Clause h b) = Clause (th `applySubst` h) (map (applySubst th) b)

		p = p' --  map (mapClause apartL) p'
		cl = cl' -- mapClause apartR cl'
		si' = osi' -- mapSubst apartR os
		ands = ands'
		term = term'
		Clause h _ = p !! pi


--der :: (Eq a, Eq b, Eq d, Ord b, Freshable b, Freshable d) =>
--	Program a b c -> Clause a b c -> DerTree a b c d
der p c = (derT p $ rew p c [])

--derT :: (Eq a, Eq b, Eq d, Ord b, Freshable b, Freshable d) =>
--	Program a b c -> RewTree a b c d -> DerTree a b c d
derT p rt = DT rt $ fmap toTrans (fmap fst' $ getVrs rt)
	where
		toTrans v = let (rt', cp) = trans p rt v in Trans p v cp  $ derT p rt' -- $ derT p rt'
		fst' (a, _, _) = a

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
	Program a b c -> Maybe (Int, Subst a b c, Term a b c) 
	-> GuardingContext a b c
clauseProj _ Nothing 		= []
clauseProj p (Just (pk, si, t))
	| Just t'' <- t `isVarReductOf` (si `applySubst` t),
	  Clause h _ <- p !! pk = do
			(t', v) <- nvPropSub h 
			_ <- maybeToList $ match t' t'' 
			return (pk, t', v) 
	| otherwise	= []

