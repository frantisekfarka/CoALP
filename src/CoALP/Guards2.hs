{-# LANGUAGE FlexibleContexts #-}
-- | This module provides guardednes checks
--
module CoALP.Guards2 (
	  gc1 -- ^ guardenes on clauses
	, gc2 -- ^ guardenes on rew trees
	, gc3 -- ^ guardenes on der trees
	, gcRewTree
	, gc3one
	, derToObs
	, depthOT
) where

import Debug.Trace

import Control.Arrow ((***))
--import Data.Functor ((<$>))
--import Data.Maybe (catMaybes)
import Data.Traversable (sequenceA,traverse)

import CoALP.Program (Program, Clause(..), Term(..),
	AndNode(..),OrNode(..),RewTree(..),
	DerTree(..),Trans(..),
	GuardingContext,
	OTree(..),OTree1,OTrans(..),OTrans1,DerTree1,Trans1,GuardingContext1,
	RewTree(..),
	RewTree(..)
	)

import CoALP.FreshVar (Freshable)
import CoALP.RewTree (rew)
import CoALP.DerTree (der,clauseProj)



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
guardedClause (Clause h b) = all (guardedTerm h) $ filter (sameHead h) b
	where
		sameHead (Fun p1 _) (Fun p2 _) 	= p1 == p2
		sameHead (Var v1) (Var v2)	= v1 == v2
		sameHead _ _			= False

-- | reflects definitoon 4.3
--
-- we ensure 2) by recursion on Term
guardedTerm :: Term a b c -> Term a b c -> Bool
guardedTerm (Fun p1 s1) (Fun p2 s2)	= p1 == p2 && -- should hold from recursive hypothesis
	any (uncurry guardedTerm) [
		(t1,t2) | t1 <- s1, t2 <- s2
	] 
guardedTerm (Fun _ []) (Var _)		= False
guardedTerm (Fun _ _) (Var _)		= True	-- ^ this is the constructor guarding productivity
guardedTerm _ _				= False




--gc2 :: (Eq a, Ord b, Freshable b) => Program a b c -> Clause a b c -> Bool
gc2 p c = gcRewTree (rew p c [])

--gcRewTree :: (Eq a, Eq b, Ord b, Freshable b, Freshable d) => RewTree a b c d -> Bool
--gcRewTree :: RewTree a b c Integer -> Bool
gcRewTree rt = (all (uncurry guardedTerm) $ (loops (rt)))
	where
		f x = case rt of
			(RT c s _) -> trace ("gcRewTree: " ++ show x ++ ", " ++ show c ++ ", " ++ show s) x
			RTEmpty	-> x
		g x = if null x then x else traceShow (take 5 x) x
		h (RTEmpty) = RTEmpty
		h x = traceShowId rt

-- TODO Freshable!
--loops :: Freshable d => RewTree a b c d -> [(Term a b c, Term a b c)]
loops :: RewTree a b c Integer -> [(Term a b c, Term a b c)]
loops rt = snd (loops' rt)

-- | recursively build loops
loops' :: Freshable d => RewTree a b c d -> ([(Term a b c,Int)],[(Term a b c, Term a b c)])
loops' RTEmpty = ([],[])
loops' (RT _ _ ands) = (id *** concat.concat) $ sequenceA $ fmap f ands
	where
		f (AndNode _ ors) = sequenceA $ zipWith loopsO [1..] ors

loopsA :: Int -> AndNode (Clause a b c) (Term a b c) d -> ([(Term a b c, Int)],[(Term a b c, Term a b c)])
loopsA pari (AndNode f@(Fun fid _) ors) = (id *** concat) $
				sequenceA $ ([(f,pari)],newLoops) : boundLower
	where
		boundLower = zipWith loopsO [1..] ors
		newLoops = [(f, f') | (f'@(Fun fid' _), pari')  <- concatMap fst boundLower,
			fid == fid' && pari == pari'
			]

loopsA _ (AndNode (Var _) ors) = (id *** concat) $ sequenceA $ zipWith loopsO [1..] ors

loopsO :: Int -> OrNode (Clause a b c) (Term a b c) d -> ([(Term a b c, Int)],[(Term a b c, Term a b c)])
loopsO _ (OrNodeEmpty _) = ([],[])
loopsO pari (OrNode _  ands) = (id *** concat) $ traverse (loopsA pari) ands



--gc3 :: (Freshable b, Ord b, Eq a) =>
--	Program a b c -> Bool
gc3 p = all (gc3one p ) p
	


--gc3one :: (Freshable b, Ord b, Eq a) =>
--	Program a b c -> Clause a b c -> Bool
gc3one p c = gcDerTree [] $ der p c

--gcDerTree :: (Freshable b, Ord b, Eq a) =>
--	DerTree a b c Integer -> Bool
--gcDerTree dt = foo [] dt

--gcDerTree :: (Eq a, Eq b, Ord b) => [GuardingContext a b c] -> DerTree a b c Integer -> Bool
gcDerTree gcs (DT rt trs) =  (gcRewTree rt) && all (gcTrans gcs) trs
	where

--gcTrans :: (Eq a, Eq b, Ord b) => [GuardingContext a b c] -> Trans a b c Integer -> Bool
gcTrans gcs (Trans p _ cx dt) = case cp `elem` gcs of
		True	-> True
		False	-> gcDerTree (cp:gcs) dt
	where
		cp = clauseProj p cx 


derToObs :: DerTree1 -> OTree1
derToObs dt = derToObs' [] dt 

derToObs' :: [GuardingContext1] -> DerTree1 -> OTree1
derToObs' gcs (DT rt trs) = case gcRewTree rt of
	False	-> UNRT rt
	True	-> ODT rt $ map (transToObs gcs) trs


transToObs :: [GuardingContext1] -> Trans1 -> OTrans1
transToObs gcs (Trans p v cx dt) = case cp `elem` gcs of
		True	-> GTrans v gcs cp
		False	-> OTrans p v cx $ derToObs'(cp:gcs) dt
	where
		cp = clauseProj p cx 


depthOT :: OTree1 -> Int
depthOT (ODT _ [])	= 1 
depthOT (ODT _ trs)	= 1 + (maximum $ map depthTrs trs)
depthOT (UNRT _)	= 0

depthTrs :: OTrans1 -> Int
depthTrs (OTrans _ _ _ ot)	= depthOT ot
depthTrs (GTrans _ _ _) 	= 0



