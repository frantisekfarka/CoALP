-- | This module provides guardednes checks
--
module CoALP.Guards2 (
	gc1 -- ^ guardenes on vlauses

	-- debug
	, guardedTerm
	, guardedClause
) where

import CoALP.Program (Program, Clause(..), Term(..))




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





