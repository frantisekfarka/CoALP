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
	, Ident, Constant, Var, VR, Program, Signature
	)

import CoALP.DerTree (der,clauseProj)
import CoALP.Guards (guardingContext)

import Debug.Trace


res ::
	Program Ident Constant Var
	-> Signature Ident
	-> Clause Ident Constant Var
	-> [Succ Ident Constant Var]
res p s c = resDerTree [] $ (der p c :: DerTree Ident Constant Var VR )


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









