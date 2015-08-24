module CoALP.Sound (
	res
) where

import CoALP.Program (
	  DerTree (..)
	, DerTree1
	, RewTree (..)
	, Trans (..)
	, Clause (..)
	, AndNode (..)
	, OrNode (..)
	, Succ (..)
	)

import CoALP.DerTree (der,clauseProj)
import CoALP.Guards (guardingContext)

import Debug.Trace


--resolve ::
--	Clause a b c
--	-> [Subst a b c]
res p c = resDerTree [] $ (der p c :: DerTree1 )


resDerTree gcs (DT rt trs) = (indRes rt) ++ (concatMap (resTrans gcs) trs)

resTrans gcs (Trans p rt _ cx dt) = case indClosed && (not $ null gc) && (gc `elem` gcs) of
		True	-> [rep rt]
		False	-> resDerTree (gc:gcs) dt
	where
		gc = guardingContext p rt cx 
		rep (RTEmpty) = error "impossible"
		rep (RT c s _) = CoIndS c gc
		indClosed = False


-- TODO make into traversal over the tree
indRes RTEmpty = []
indRes (RT c _ ands) = if any hasSuccTreeAnd ands then [IndS c] else []

		-- concatMap (indResAnds c) ands
	where
		hasSuccTreeAnd (AndNode _ ors) = any hasSuccTreeOr ors
		hasSuccTreeOr (OrNodeEmpty _) = False
		hasSuccTreeOr (OrNode a []) = True
		hasSuccTreeOr (OrNode _ ands) = all hasSuccTreeAnd ands









