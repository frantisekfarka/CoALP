-- | Module that constructs rewriting tree
module CoALP.DerTree (
	  der
	, trans
	, mkVar
	, clauseProj
) where

import Data.Maybe (maybeToList)

import CoALP.RewTree (rew, getVrs)
import CoALP.FreshVar (Freshable,apartR, apartL)
import CoALP.Unify (unify, applySubst, composeSubst, match)
import CoALP.Program (Program, Clause(..), Subst, RewTree(..), DerTree(..),
	Term(..),Vr(..),mkVar, Trans(..),
	GuardingContext, mapClause,
	mapProg,

	)
import CoALP.Reductions (isVarReductOf,nvPropSub)


--import Debug.Trace

-- | compute the rew tree transition
-- TODO make sure this works for infinite tree
--
-- TODO moce makeVrs to derT
--
trans :: (Eq a, Eq b, Ord b, Eq d, Freshable b, Freshable d)
	=> Program a b c -> RewTree a b c d -> Vr d ->
	(RewTree a b c d, Maybe (Int, Subst a b c, Term a b c))
trans _ RTEmpty _ = (RTEmpty, Nothing)
trans p rt@(RT cl si' _) vr = case term `unify` h of
		Just si	-> (mkRew (si `composeSubst` upds si)
			, Just (pk, si' `composeSubst` (si `composeSubst` upds si)
			, term))  -- TODO comopse necessarily?
		Nothing -> (RTEmpty, Nothing)
	where
		mkRew th = rew p (th `claps` cl) (th `composeSubst` si')
		(_, term, pk):_ = filter ((== vr).fst') $ getVrs rt
		fst' (a,_,_) = a
		claps th (Clause h' b') = Clause (th `applySubst` h') (map (applySubst th) b')
		Clause h _ = p !! pk
		upds _ = [] -- mapSubst (apartL . unpart) th


--der :: (Eq a, Eq d, Ord b, Freshable b, Freshable d) =>
--	Program a b c -> Clause a b c -> DerTree a b c d
der p c = (derT p p' $ rew p' c' [])
	where
		p' = mapProg apartL p
		c' = mapClause apartR c

--derT :: (Eq a, Eq d, Ord b, Freshable b, Freshable d) =>
--	Program a b c -> RewTree a b c d -> DerTree a b c d
derT p0 p rt = DT rt $ fmap toTrans (fmap fst' $ getVrs rt')
	where
		
		toTrans v = let (rt'', cp) = trans p' rt' v 
			in Trans p0 rt' v cp  $ derT p0 p' rt'' 
		fst' (a, _, _) = a
		p' = p -- mapProg apartL p
		rt' = rt -- mapRT apartR rt

clauseProj :: (Eq a, Ord b) => 
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

	where
		{- f x = if pk == 2 then (
			trace ("ClauseProj of \n\t" ++ show t ++ "\n\t" ++ show si ++
			"\n\ttrying " ++ show (si `applySubst` t))  x
			) else x
		-}

