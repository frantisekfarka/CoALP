-- | This module provides guardednes checks
--
module CoALP.Guards2 (
	  gc1 -- ^ guardenes on clauses
	, gc2 -- ^ guardenes on rew trees

	-- debug
	, guardedTerm
	, guardedClause
	, loops'
) where

import Control.Arrow ((***))
import Data.Functor ((<$>))
import Data.Maybe (catMaybes)
import Data.Traversable (sequenceA,traverse)

import CoALP.Program (Program, Clause(..), Term(..),
	AndNode(..),OrNode(..),RewTree(..),
	Query(..)
	)

import CoALP.FreshVar (Freshable)
import CoALP.RewTree (rew)




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
-- TODO heads to tree lanگ projections?
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




gc2 :: Query a b c -> Program a b c -> Bool
gc2 q p = all (uncurry guardedTerm) $ loops (rew p q [])

-- TODO Freshable!
--loops :: Freshable d => RewTree a b c d -> [(Term a b c, Term a b c)]
loops :: RewTree a b c Integer -> [(Term a b c, Term a b c)]
loops rt = snd (loops' rt)

-- | recursively build loops
loops' :: Freshable d => RewTree a b c d -> ([(Term a b c,Int)],[(Term a b c, Term a b c)])
loops' (RT _ _ ands) = (id *** concat.concat) $ sequenceA $ fmap f ands
	where
		f (AndNode _ ors) = sequenceA $ zipWith loopsO [1..] ors

loopsA :: Int -> AndNode (Clause a b c) (Term a b c) d -> ([(Term a b c, Int)],[(Term a b c, Term a b c)])
loopsA pi (AndNode f@(Fun fid _) ors) = (id *** concat) $
				sequenceA $ ([(f,pi)],newLoops) : boundLower
	where
		boundLower = zipWith loopsO [1..] ors
		newLoops = [(f, f') | (f'@(Fun fid' _), pi')  <- concatMap fst boundLower,
			fid == fid' && pi == pi'
			]

loopsA pi (AndNode (Var _) ors) = (id *** concat) $ sequenceA $ zipWith loopsO [1..] ors

loopsO :: Int -> OrNode (Clause a b c) (Term a b c) d -> ([(Term a b c, Int)],[(Term a b c, Term a b c)])
loopsO pi (OrNodeEmpty _) = ([],[])
loopsO pi (OrNode _  ands) = (id *** concat) $ traverse (loopsA pi) ands



