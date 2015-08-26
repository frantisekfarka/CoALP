-- | Construction of a derivation tree 
module CoALP.DerTree (
	-- * Tree constructions
	  der
	, trans
	, clauseProj

	-- * Rewritng tree variable
	, Vr(..)
) where

import Data.Maybe (maybeToList)

import CoALP.RewTree (rew, getVrs,extrew)
import CoALP.FreshVar (Freshable,apartR, apartL)
import CoALP.Unify (unify, applySubst, composeSubst, match)
import CoALP.Program (Program, Clause(..), Subst, RewTree(..), DerTree(..),
	Term(..),Vr(..),Trans(..),
	GuardingContext, 
	)
import CoALP.Reductions (isVarReductOf,nvPropSub)



-- | Given a program P, rewriting tree rew(P, C, &#x3c3;) at position w and rewriting
-- tree variable X_k construct the rewritng tree 
--
-- @
--	Trans(P, D(w), X_k) = rew(P, C, &#x3c3;'&#x3c3;)
-- @
--
-- and return the external resolvent &#x3c3;, contraction measure, and the
-- originating clause according to @Definition 3.5@
--
trans :: (Eq a, Eq b, Ord c, Eq d, Freshable c, Freshable d)
	=> Program a b c -> RewTree a b c d -> Vr d ->
	(RewTree a b c d, Maybe (Int, Subst a b c, Term a b c))
trans _ RTEmpty _ = (RTEmpty, Nothing)
trans p rt@(RT _ si' _) vr = case term `unify` h of
		Just si	-> (mkRew si
			, Just (pk, si' `composeSubst` si
			, term)) 
		Nothing -> (RTEmpty, Nothing)
	where
		mkRew th = extrew p rt th
		(_, term, pk):_ = filter ((== vr).fst') $ getVrs rt
		fst' (a,_,_) = a
		Clause h _ = p !! pk

-- | Given a program P and a clause C construct derivation tree
--
-- @
-- 	der(P, C)
-- @
--
-- according to definition 3.6
--
der :: (Eq a, Eq b, Eq d, Ord c, Freshable c, Freshable d) =>
	Program a b c -> Clause a b c -> DerTree a b c d
der p c = (derT p p' $ rew p' c' [])
	where
		p' = fmap (fmap apartL) p
		c' = fmap apartR c

-- | Compute next Derivation tree after transition
derT :: (Eq a, Eq b, Eq d, Ord c, Freshable c, Freshable d) =>
	Program a b c -> 
	Program a b c -> 
	RewTree a b c d ->
	DerTree a b c d
derT p0 p rt = DT rt $ fmap toTrans (getVrs rt')
	where
		
		toTrans (v, t, _) = let (rt'', cp) = trans p' rt' v 
			in Trans p0 rt' v (getIden t) cp $ derT p0 p' rt'' 
		fst' (a, _, _) = a
		p' = fmap (fmap apartR) p
		rt' = rt -- first apartR rt
		getIden (Fun a _) = a
		getIden _	= error "Impossibru"

-- | Given program P and external resolvent of a transition with its measure
-- conpute clause projection
--
-- @
-- 	&#x3c0;(D(w)) = {P(k), t', v | ... }
-- @
--
-- accorging to definition 5.4
clauseProj :: (Eq a, Eq b, Ord c) => 
	Program a b c -> Maybe (Int, Subst a b c, Term a b c) 
	-> GuardingContext a b c
clauseProj _ Nothing 		= []
clauseProj p (Just (pk, si, t))
	| Just t'' <- -- traceShow (t, si `applySubst` t) $  traceShowId $ 
		t `isVarReductOf` (si `applySubst` t),
	  Clause h _ <- p !! pk = [ (pk, t', v) |
			(t', v) <- nvPropSub h 
			, _ <- maybeToList $ match t' t'' 
			]
	| otherwise	= []


