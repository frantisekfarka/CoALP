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


--resolve ::
--	Clause a b c
--	-> [Subst a b c]
res p c = resDerTree [] $ (der p c :: DerTree1 )


resDerTree gcs (DT rt trs) = (indRes rt) ++ (concatMap (resTrans gcs) trs)

resTrans gcs (Trans p rt _ cx dt) = case (not $ null gc) && (gc `elem` gcs) of
		True	-> [rep rt]
		False	-> resDerTree (gc:gcs) dt
	where
		gc = guardingContext p rt cx 
		rep (RTEmpty) = error "impossible"
		rep (RT c s _) = CoInd c



indRes RTEmpty = []
indRes (RT _ _ ands) = concatMap indResAnds ands
	where
		indResAnds (AndNode _ ors) = concatMap indResOrs ors
		indResOrs (OrNodeEmpty _) = []
		indResOrs (OrNode a []) = [Ind a]
		indResOrs (OrNode a ands) = concatMap indResAnds ands





