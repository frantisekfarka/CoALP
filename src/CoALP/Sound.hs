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
	)

import CoALP.DerTree (der)
import CoALP.Guards (guardingContext)
import CoALP.FreshVar (Freshable)


-- | Resolution on clause
--
-- according to paper 1
res :: (Eq a, Eq b, Ord c, Freshable c) =>
	Program a b c
	-> Clause a b c
	-> [Succ a b c]
res p c = resDerTree [] $ der p c 

-- | Resolution on der tree
--
resDerTree :: (Eq a, Eq b, Ord c) =>
	[GuardingContext a b c]
	-> DerTree a b c VR 
	-> [Succ a b c]
resDerTree gcs (DT rt trs) = (indRes rt) ++ (concatMap (resTrans gcs) trs)

-- | Resolution on der tree - co-inductive observations
--
resTrans :: (Eq a, Eq b, Ord c) =>
	[GuardingContext a b c]
	-> Trans a b c VR 
	-> [Succ a b c]
resTrans gcs (Trans p rt _ cx dt) = case (not $ null gc) && (gc `elem` gcs) of
		True	-> [rep rt]
		False	-> resDerTree (gc:gcs) dt
	where
		gc = guardingContext p rt cx 
		rep (RTEmpty) = error "impossible"
		rep (RT c _ _) = CoInd c



-- | Resolution on rew tree - inductive observations
--
indRes :: RewTree a b c d -> [Succ a b c]
indRes RTEmpty = []
indRes (RT c _ ands) = concatMap (indResAnds c) ands
	where
		indResAnds c' (AndNode _ ors) = concatMap (indResOrs c') ors
		indResOrs _ (OrNodeEmpty _) = []
		indResOrs c' (OrNode _ []) = [Ind c']
		indResOrs c' (OrNode _ ands') = concatMap (indResAnds c') ands'





