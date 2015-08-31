{-# LANGUAGE ScopedTypeVariables #-}

-- | 
-- Soundness results for query resolution according to the paper 1
module CoALP.Sound (
	res
) where

import CoALP.Program (
	  DerTree (..)
	, RewTree (..)
	, Trans (..)
	, AndNode (..)
	, OrNode (..)
	, Succ (..)
	, VR 
	, Program
	, Clause
	, GuardingContext
	, VR
	, Program
	, Signature
	, GuardingContext
	, lookupType
	, Type(..)
	)

import CoALP.DerTree (der)
import CoALP.Guards (guardingContext)
import CoALP.FreshVar (Freshable)


-- | Resolution on clause
--
-- according to paper 1
res :: forall a b c . (Eq a, Show a, Show b, Show c, Ord a, Eq b, Ord c, Freshable c) =>
	Program a b c
	-> Signature a
	-> Clause a b c 
	-> [Succ a b c]
res p s c = resDerTree s [] dt
	where
		dt :: DerTree a b c VR 
		dt = der p c 

-- | Process a der tree and continue
--
resDerTree :: (Show a, Show b, Show c, Ord a, Eq b, Ord c) =>
	Signature a
	-> [GuardingContext a b c]
	-> DerTree a b c t
	-> [Succ a b c]
resDerTree sig gcs (DT rt trs) = (indRes rt) ++
	case separateTrs sig trs of
		([], cotrs)	-> concatMap (resCoIndTrans sig gcs) cotrs
		(indtrs, _)	-> concatMap (resIndTrans sig gcs) indtrs


-- | Process a transition within a tree that sill has
-- some unprocessed inductive obligations
resIndTrans :: (Show a, Show b, Show c, Ord a, Eq b, Ord c) =>
	Signature a
	-> [GuardingContext a b c]
	-> Trans a b c d
	-> [Succ a b c]
resIndTrans sig gcs (Trans p rt _ _ cx dt) = resDerTree sig (gc:gcs) dt
	where
		gc = guardingContext p rt cx 

-- | Process a transition within a tree that has no inductive
-- obligations - therefore we can conclude coinductively
resCoIndTrans :: (Show a, Show b, Show c, Ord a, Eq b, Ord c) =>
	Signature a
	-> [GuardingContext a b c]
	-> Trans a b c d
	-> [Succ a b c]
resCoIndTrans sig gcs (Trans p rt _ _ cx dt) = case (not $ null gc) && (gc `elem` gcs) of
		True	-> [rep rt]
		False	-> resDerTree sig (gc:gcs) dt
	where
		gc = guardingContext p rt cx 
		rep (RTEmpty) = error "impossible"
		rep (RT c _ _) = CoIndS c gc

-- | Separate transitions into inductive and coinductive obligations
separateTrs :: Ord a => 
	Signature a
	-> [Trans a b c d]
	-> ([Trans a b c d], [Trans a b c d])
separateTrs sig trs = foldr f ([], []) trs
	where
		f t@(Trans _ _ _ i _ _) (as,bs) = case lookupType sig i of
			SInd	-> (t:as, bs)
			SCoInd	-> (as, t:bs)

-- | Resolution on rew tree - inductive observations
--
-- TODO make into traversal over the tree
indRes :: RewTree a b c d -> [Succ a b c]
indRes RTEmpty = []
indRes (RT c _ ands) = if any hasSuccTreeAnd ands then [IndS c] else []

	-- concatMap (indResAnds c) ands
	where
		hasSuccTreeAnd (AndNode _ ors) = any hasSuccTreeOr ors
		hasSuccTreeOr (OrNodeEmpty _) = False
		hasSuccTreeOr (OrNode _ []) = True
		hasSuccTreeOr (OrNode _ ands') = all hasSuccTreeAnd ands'



