{-# LANGUAGE ScopedTypeVariables #-}

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
import CoALP.FreshVar (Freshable)
import CoALP.Guards (guardingContext)

import Debug.Trace


-- | Resolve a query
--
res :: forall a b c . (Eq a, Eq b, Ord c, Freshable c) =>
	Program a b c
	-> Signature a
	-> Clause a b c 
	-> [Succ a b c]
res p s c = resDerTree [] dt
	where
		dt :: DerTree a b c VR 
		dt = der p c 

-- | Process a tree and procees
--
resDerTree gcs (DT rt trs) = (indRes rt) ++
	case separateTrs trs of
		([], cotrs)	-> concatMap (resCoIndTrans gcs) cotrs
		(indtrs, _)	-> concatMap (resIndTrans gcs) indtrs


-- | Process a transition within a tree that sill has
-- some unprocessed inductive obligations
resIndTrans gcs (Trans p rt _ cx dt) = resDerTree (gc:gcs) dt
	where
		gc = guardingContext p rt cx 

-- | Process a transition within a tree that has no inductive
-- obligations - therefore we can conclude coinductively
resCoIndTrans gcs (Trans p rt _ cx dt) = case (not $ null gc) && (gc `elem` gcs) of
		True	-> [rep rt]
		False	-> resDerTree (gc:gcs) dt
	where
		gc = guardingContext p rt cx 
		rep (RTEmpty) = error "impossible"
		rep (RT c s _) = CoIndS c gc

-- | Separate transitions into inductive and coinductive obligations
separateTrs trs = ([], trs)




-- TODO make into traversal over the tree
indRes RTEmpty = []
indRes (RT c _ ands) = if any hasSuccTreeAnd ands then [IndS c] else []

		-- concatMap (indResAnds c) ands
	where
		hasSuccTreeAnd (AndNode _ ors) = any hasSuccTreeOr ors
		hasSuccTreeOr (OrNodeEmpty _) = False
		hasSuccTreeOr (OrNode a []) = True
		hasSuccTreeOr (OrNode _ ands) = all hasSuccTreeAnd ands









