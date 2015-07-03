{-# LANGUAGE FlexibleContexts #-}
-- | This module provides guardednes checks
--
module CoALP.Guards (
	  gc1 -- guardenes on clauses
	, gc2 -- guardenes on rew trees
	, gc3 -- guardenes on der trees
	, gcRewTree
	, gc3one
	, derToObs
	, derToUnc
	, depthOT
	, guardingContext
	, guardedTerm, recGuardedTerm
) where

--import Control.Arrow ((***))
import Data.Functor ((<$>))
import Data.Foldable (asum)
import Data.List (nub)
import Data.Maybe (maybeToList)
-- import Data.Traversable (sequenceA,traverse)

import CoALP.Program (Program, Clause(..), Term(..),
	AndNode(..),OrNode(..),RewTree(..),
	DerTree(..),Trans(..),
	GuardingContext,
	OTree(..),OTree1,OTrans(..),OTrans1,DerTree1,Trans1,GuardingContext1,
	RewTree(..),
	RewTree(..),
	mapClause,
	Loop,Subst,
	subtermOf,propSubtermOf
	)

import CoALP.FreshVar (Freshable,apartL,apartR)
import CoALP.RewTree (rew)
import CoALP.DerTree (der,clauseProj)
import CoALP.Unify (match)

-- | GC1 -- guardendes on cluases
-- GQ 1 requires that:
--
-- for every program P and j ∈ dom(P(i)), P(i)(ε).P(i)(j)
-- whenever P(i)(ε)(ε) = P(i)(j)(ε). 
--
-- TODO move this to definition of program
--
-- notation program P over Σ is total function
-- {0..n} ε N to non-goal clauses
--
-- P(i) is n-th clause
--
-- clause C over Σ is a total function from finite tree language L
-- of depth 1 to terms (term trees)
--
-- C(ε) is a head of clause
-- C(i) is a n-th term (for i =/= ε)
--
-- Term
-- ...
-- 
-- T(ε) is a predicate of term
-- 
--
gc1 :: Program a b c -> Bool
gc1 = all guardedClause



-- | Check that clause is guarded
--
-- i. e. 
--
-- whenever C(ε) and C(i) have same predicate check that
--
-- C(i) is a reduct of C(ε)
--
-- TODO heads to tree lang projections?
guardedClause :: Clause a b c -> Bool
guardedClause (Clause h b) = all (isJust . guardedTerm h) $ filter (sameHead h) b
	where
		sameHead (Fun p1 _) (Fun p2 _) 	= p1 == p2
		sameHead (Var v1) (Var v2)	= v1 == v2
		sameHead _ _			= False
		isJust (Just _)	= True
		isJust _	= False

-- | reflects definitoon 5.1
--
-- we ensure 2) by recursion on Term
guardedTerm :: Term a b c -> Term a b c -> Maybe (Term a b c)
guardedTerm (Fun p1 s1) (Fun p2 s2)	= if p1 == p2 -- should hold from recursive hypothesis
		then (asum $ fmap (uncurry guardedTerm) [
			(t1,t2) | t1 <- s1, t2 <- s2
			])
		else Nothing
guardedTerm (Fun _ []) (Var _)		= Nothing
guardedTerm t@(Fun _ s) (Var _)		= if length s > 0 
		then Just t	-- this is the constructor guarding productivity
		else Nothing
guardedTerm _ _				= Nothing


-- | repeated definition 5.1 for recursive contraction
--
--   the direction is v -> v (w is a prefix of v)
recGuardedTerm :: (Eq a, Eq b) =>  Term a b c -> Term a b c -> Maybe (Term a b c)
recGuardedTerm f@(Fun _ _) v@(Var _)	= case v `subtermOf` f of
	--True	-> Just f
	True	-> Just f
	False	-> Nothing

recGuardedTerm f@(Fun _ _) c@(Fun _ [])	= case c `propSubtermOf` f of
	True	-> Just f
	False	-> Nothing

recGuardedTerm (Fun p1 s1) (Fun p2 s2)	= if p1 == p2 -- should hold from recursive hypothesis
		then (f $ fmap (uncurry recGuardedTerm) [
			(t1,t2) | t1 <- s1, t2 <- s2
			])
		else Nothing
	where
		-- asum?
		f [] = Nothing
		f ((Just t):_) = Just t
		f (Nothing:xs) = f xs
recGuardedTerm _ _			= Nothing

-- TODO
recGuardedTermB :: (Eq a, Eq b) =>  Term a b c -> Term a b c -> Bool
recGuardedTermB t1 t2 = case recGuardedTerm t1 t2 of
	Just _	-> True
	Nothing	-> False

gc2 :: (Eq a, Ord b, Freshable b) => Program a b c -> Clause a b c -> Bool
gc2 p c = gcRewTree (rew p' c' []) 
	where
		p' = map (mapClause apartL) p
		c' = mapClause apartR c

gcRewTree :: (Eq a, Eq b) =>  RewTree a b c Integer -> Bool
gcRewTree RTEmpty	= True
gcRewTree rt@(RT _ _ _) = all (uncurry recGuardedTermB . g) $ (loops rt)
	where
		g (t1,t2,_) = (t1,t2)

gc3 :: (Freshable b, Ord b, Eq a) =>
	Program a b c -> Bool
gc3 p = all (gc3one p ) p
	


gc3one :: (Freshable b, Ord b, Eq a) =>
	Program a b c -> Clause a b c -> Bool
gc3one p c = gcDerTree [] $ der p c

gcDerTree :: (Eq a, Eq b, Ord b) => [GuardingContext a b c] -> DerTree a b c Integer -> Bool
gcDerTree gcs (DT rt trs) =  (gcRewTree rt) && all (gcTrans gcs) trs

gcTrans :: (Eq a, Eq b, Ord b) => [GuardingContext a b c] -> Trans a b c Integer -> Bool
gcTrans gcs (Trans p rt _ cx dt) = case (not $ null gc) && (gc `elem` gcs) of
		True	-> True
		False	-> gcDerTree (gc:gcs) dt
	where
		gc = guardingContext p rt cx 
		--gc = trace ("GC: " ++ show gc' ++ "\n\t" ++ show gcs ++
		--	"\n\t" ++ show (gc' `elem` gcs)) gc'


derToObs :: DerTree1 -> OTree1
derToObs dt = derToObs' [] dt 

derToObs' :: [GuardingContext1] -> DerTree1 -> OTree1
derToObs' gcs (DT rt trs) = case gcRewTree rt of
	False	-> UNRT rt
	True	-> ODT rt $ map (transToObs gcs) trs


transToObs :: [GuardingContext1] -> Trans1 -> OTrans1
transToObs gcs (Trans p rt v cx dt) = case (not $ null gc) && (gc `elem` gcs) of
		True	-> GTrans v gcs gc
		False	-> OTrans p rt v cx $ derToObs'(gc:gcs) dt
	where
		gc = guardingContext p rt cx 

derToUnc :: Int -> DerTree1 -> DerTree1
derToUnc n dt@(DT rt _) = case derToUnc' n [] dt of
	Just dt'	-> dt'
	Nothing		-> DT rt []
	

derToUnc' :: Int -> [GuardingContext1] -> DerTree1 -> Maybe DerTree1
derToUnc' 0 _   (DT rt _) = Just $ DT rt []
derToUnc' n gcs (DT rt trs) = case gcRewTree rt of
		False	-> Nothing -- we found unguarded tree and thus we can finish
		True	-> (DT rt) <$> (altseq $ map (transToUnc (n-1) gcs) trs)
	where
		altseq xs = let r = altseq' xs in if null r then Nothing else Just r
		altseq' ((Just x):_) = [x] -- :(altseq' xs)
		altseq' (Nothing:xs) = altseq' xs
		altseq' [] = []

transToUnc :: Int -> [GuardingContext1] -> Trans1 -> Maybe (Trans1)
transToUnc n gcs (Trans p rt v cx dt) = case (not $ null gc) && (gc `elem` gcs) of
		True	-> Nothing -- guarded trs
		False	-> (Trans p rt v cx) <$> derToUnc' n (gc:gcs) dt
	where
		gc = guardingContext p rt cx 





depthOT :: OTree1 -> Int
depthOT (ODT _ [])	= 1 
depthOT (ODT _ trs)	= 1 + (maximum $ map depthTrs trs)
depthOT (UNRT _)	= 0

depthTrs :: OTrans1 -> Int
depthTrs (OTrans _ _ _ _ ot)	= depthOT ot
depthTrs (GTrans _ _ _) 	= 0




guardingContext :: (Eq a, Ord b) =>
	Program a b c
	-> RewTree a b c d
	-> Maybe (Int, Subst a b c, Term a b c) 
	-> GuardingContext a b c
guardingContext p rt cx	= nub [(pkt', t', v) |
		(pkt', t', v) <- -- trace "\n\nnextcmp" $ traceShowId $
			clauseProj p cx
		, (t1, t2, pkt'') <- -- f t' $
			(loops rt)
		, t'' <- --trace ("loop:\n\t" ++ show t1  ++ "\n\t" ++ show t2) $
			maybeToList $ recGuardedTerm t1 t2
		, -- trace "It's guarded!" $ traceShow t'' $ 
			pkt' == pkt'' && isJust (t' `match` t'')
		--, pkt' == pkt'' && (
		--,
		--	traceShow (pkt', pkt'', t', t'', isJust (t' `match` t'')) $
		--	True)
		]
	where
		isJust (Just _) = True
		isJust _	= False


-- TODO
-- check, whether  we can use slightly different version here
-- in out version: loops are all pairs of terms in a branch that
-- originate from the same clause
--
-- in the paper, loops are pairs of therms, that
--	1/ are in the same branch (w prefix of v)
--	2/ have same head
--	3/ parents can be matched to the same program clause
--
loops :: RewTree a b c d -> [Loop a b c] 
loops (RTEmpty)	= []
loops (RT _ _ ands) = concat $ concatMap loops0 ands
	where
		loops0 (AndNode _ ors) = concat $ zipWith (oLoops []) [0..] ors

	


aLoops :: 
	[(Term a b c, (Int, Int))]
	-> Int -- ^ parent clause ix
	-> Int -- ^ term ix
	-> AndNode (Clause a b c) (Term a b c) d
	-> [[Loop a b c]]
aLoops tws pci ti (AndNode t ors) = (concatMap f tws) : 
		(concat $ zipWith (oLoops ((t, (pci, ti)):tws)) [0..] ors)
	where
		f (t', (pci', _ti')) = if pci' == pci && eqs t t'
			then [(t', t, pci)]
			else []
		eqs (Fun t1 _)	(Fun t2 _)	= t1 == t2
		eqs _		_		= False

oLoops :: 
	[(Term a b c, (Int, Int))]
	-> Int -- ^ clause ix
	-> OrNode (Clause a b c) (Term a b c) d
	-> [[Loop a b c]]
oLoops _	_	(OrNodeEmpty _) = []
oLoops tws	ci	(OrNode _ ands) = concat $ zipWith (aLoops tws ci) [0..] ands 



