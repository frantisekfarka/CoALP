{-# LANGUAGE FlexibleContexts #-}
-- | Guardednes checks
--
module CoALP.Guards (
	-- * Guardedness checks
	  gc1 -- guardenes on clauses
	, gc2 -- guardenes on rew trees
	, gc3 -- guardenes on der trees
	, gc3one
        , getProgramLoops

	-- * Guarding context
	, guardingContext

	-- * GC on a rewriting tree
	, gcRewTree

	-- * Derivation tree transformations
	, derToObs
	, derToUnc
	, derToUng
) where

--import Control.Arrow ((***))
import Data.Functor ((<$>))
import Data.Foldable (asum)
import Data.List (nub)
import Data.Maybe (maybeToList, catMaybes)

import CoALP.Program (Program, Clause(..), Term(..),
	AndNode(..),OrNode(..),RewTree(..),
	DerTree(..),Trans(..),
	GuardingContext,
	OTree(..),OTree1,OTrans(..),OTrans1,DerTree1,Trans1,GuardingContext1,
	RewTree(..),
	RewTree(..),
	Loop,Subst
	, subtermOf
	, propSubtermOf
	, DerTree1
	, VR
	)

import CoALP.FreshVar (Freshable,apartL,apartR)
import CoALP.RewTree (rew)
import CoALP.DerTree (der,clauseProj)
import CoALP.Unify (match)

import Control.DeepSeq (force)
import Debug.Trace

-- | Guardendes check GC1 on clauses
--
-- Given program P check that all program clauses are guarded, i. e. 
-- whenever @C(ε)@ and @C(i)@ have same predicate check that @C(i)@ is a reduct of @C(ε)@
gc1 :: (Eq a, Eq c) => Program a b c -> Bool
gc1 = all guardedClause



-- | Check that clause is guarded, i. e. 
--
-- whenever C(ε) and C(i) have same predicate check that
--
-- C(i) is a reduct of C(ε)
--
guardedClause :: (Eq a, Eq c) => Clause a b c -> Bool
guardedClause (Clause h b) = all (isJust . guardedTerm h) $ filter (sameHead h) b
	where
		sameHead (Fun p1 _) (Fun p2 _) 	= p1 == p2
		sameHead (Var v1) (Var v2)	= v1 == v2
		sameHead _ _			= False
		isJust (Just _)	= True
		isJust _	= False

-- | Checks whther a term is guurded, reflects @Definitoon 5.1@
--
-- we ensure 2) by recursion on Term
guardedTerm :: Eq a => Term a b c -> Term a b c -> Maybe (Term a b c)
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

-- | Compute recursive contraction measure on a term
-- accorging to @Definiton 5.1@
--
recGuardedTerm :: (Eq a, Eq b, Eq c) =>  Term a b c -> Term a b c -> Maybe (Term a b c)
recGuardedTerm f@(Fun _ _) v@(Var _)	= case v `subtermOf` f of
	True	-> Just f
	False	-> Nothing

recGuardedTerm f@(Fun _ _) c@(Fun _ [])	= case c `propSubtermOf` f of
	True	-> Just f
	False	-> Nothing

recGuardedTerm (Fun p1 s1) (Fun p2 s2)	= if p1 == p2 -- holds from recursive hypothesis
		then (f $ fmap (uncurry recGuardedTerm) 
			(zip s1 s2)) 
			--[
			--(t1,t2) | t1 <- s1, t2 <- s2
			--])
		else Nothing
	where
		-- asum
		f [] = Nothing
		f ((Just t):_) = Just t
		f (Nothing:xs) = f xs
recGuardedTerm _ _			= Nothing

-- | Boolean helper
recGuardedTermB :: (Eq a, Eq b, Eq c) =>  Term a b c -> Term a b c -> Bool
recGuardedTermB t1 t2 = case recGuardedTerm t1 t2 of
	Just _	-> True
	Nothing	-> False

-- | Guardedness check GC2
--
-- Given program P and a clause C check guardednees of the rewriting tree
--
-- @
-- 	rew (P, C, /id/)
-- @
gc2 :: (Eq a, Eq b, Eq c, Ord c, Freshable c) => Program a b c -> Clause a b c -> Bool
gc2 p c = gcRewTree (rew p' c' []) 
	where
		p' = force $ fmap (fmap apartL) p
		c' = force $ fmap apartR c

-- | Check GC2 guardedness of a rewritng tree
gcRewTree :: (Eq a, Eq b, Eq c) =>  RewTree a b c Integer -> Bool
gcRewTree RTEmpty	= True
gcRewTree rt@(RT _ _ _) = all (uncurry recGuardedTermB . g) $ (loops rt)
	where
		g (t1,t2,_) = (t1,t2)

-- | Guradedness check GC3 according to @Defintion 5.6@ for the whole program
gc3 :: (Freshable c, Ord c, Eq a, Eq b) =>
	Program a b c -> Bool
gc3 p = all (gc3one p ) p
	


-- | Guradedness check GC3 according to @Defintion 5.6@ for a single clause
gc3one :: (Freshable c, Ord c, Eq a, Eq b) =>
	Program a b c -> Clause a b c -> Bool
gc3one p c = gcDerTree [] $ (der p c) 

gcDerTree :: (Eq a, Eq b, Ord c) => [GuardingContext a b c] -> DerTree a b c VR -> Bool
gcDerTree gcs (DT rt trs) =  (gcRewTree rt) && all (gcTrans gcs) trs


-- | Check whether a transition id guarded
gcTrans :: (Eq a, Eq b, Ord c) => [GuardingContext a b c] -> Trans a b c Integer -> Bool
gcTrans gcs (Trans p rt _ _ cx dt) = case (not $ null gc) && (gc `elem` gcs) of
		True	-> True
		False	-> gcDerTree (gc:gcs) dt
	where
		gc = guardingContext p rt cx 

-- | Construct observation tree from a derivation tree 
-- according to the @Definition 5.5@
derToObs :: DerTree1 -> OTree1
derToObs dt = derToObs' [] dt 

-- | The actual implementation
derToObs' :: [GuardingContext1] -> DerTree1 -> OTree1
derToObs' gcs (DT rt trs) = case gcRewTree rt of
	False	-> UNRT rt
	True	-> ODT rt $ map (transToObs gcs) trs

-- | Ditto for a transition
transToObs :: [GuardingContext1] -> Trans1 -> OTrans1
transToObs gcs (Trans p rt v _ cx dt) = case (not $ null gc) && (gc `elem` gcs) of
		True	-> GTrans v gcs gc
		False	-> OTrans p rt v cx $ derToObs'(gc:gcs) dt
	where
		gc = guardingContext p rt cx 

-- | Select leftmost branch of a derivation tree that is not closed
-- up to given depth

derToUnc :: (Eq a, Eq b, Ord c) => Int -> DerTree a b c Integer -> DerTree a b c Integer
derToUnc n dt@(DT rt _) = case derToUnc' [] n [] dt 0 of
	Just dt'	-> dt'
	Nothing		-> DT rt []
	
-- | The actual implementation
derToUnc' :: (Eq a, Eq b, Ord c) => [Int] -> Int -> [GuardingContext a b c] -> DerTree a b c Integer -> Int -> Maybe (DerTree a b c Integer)
derToUnc' _    0 _   (DT rt _) _ = Just $ DT rt []
derToUnc' path n gcs (DT rt trs) tix = case gcRewTree rt of
		False	-> -- trace ("Unguarded tree at " ++ (show $ reverse (tix:path))) $
			Nothing -- we found unguarded tree and thus we can finish
		True	-> -- trace ("Continue " ++ (show $ reverse $ tix:path)) 
			(DT rt) <$> (altseq $ zipWith (transToUnc (tix:path) (n-1) gcs) trs [0..])
	where
		altseq xs = let r = altseq' xs in if null r then Nothing else Just r
		altseq' ((Just x):_) = [x] -- :(altseq' xs)
		altseq' (Nothing:xs) = altseq' xs
		altseq' [] = []

-- | Ditto for trans
transToUnc :: (Eq a, Eq b, Ord c) => [Int] -> Int -> [GuardingContext a b c] -> Trans a b c Integer ->  Int -> Maybe (Trans a b c Integer)
transToUnc path n gcs (Trans p rt v i cx dt) pix = case (not $ null gc) && (gc `elem` gcs) of
		True	-> trace ("Guarded trans at " ++ (show $ reverse path)) $ 
			Nothing -- guarded trs
		False	-> (Trans p rt v i cx) <$> derToUnc' path n (gc:gcs) dt pix
	where
		gc = guardingContext p rt cx 
-- | Select leftmost branch containing an unguarded rewriting tree in the 
-- given or less depth
derToUng :: (Eq a, Eq b, Ord c) => Int -> DerTree a b c Integer -> DerTree a b c Integer
derToUng depthD dt@(DT _ _) = case derToUng' depthD [] dt of
	Just dt'	-> dt'
	Nothing		-> DT RTEmpty []

-- | The actual implementation
derToUng' :: (Eq a, Eq b, Ord c) => Int -> [GuardingContext a b c] -> DerTree a b c Integer -> Maybe (DerTree a b c Integer)
derToUng' 0 _ (DT rt _) = Just (DT rt [])
derToUng' n gcs (DT rt trs) = case gcRewTree rt of
		False	-> Just $ DT rt []
		True	-> (DT rt) <$> (altseq $ map (transToUng (n - 1) gcs) trs)
	where
		altseq []		= Nothing
		altseq ((Just x):_)	= Just [x]
		altseq (Nothing:xs)	= altseq xs

-- | Ditto for trans
transToUng :: (Eq a, Eq b, Ord c) => Int -> [GuardingContext a b c] -> Trans a b c Integer -> Maybe (Trans a b c Integer)
transToUng n gcs (Trans p rt v i cx dt) = case (not $ null gc) && (gc `elem` gcs) of
		True	-> Nothing -- guarded trans
		False	-> (Trans p rt v i cx) <$> derToUng' n (gc:gcs) dt
	where
		gc = guardingContext p rt cx 


-- | Given a program P, rewriting tree @D(wi) = rew(P, C, &#x3c3;)@ and the external
-- resolvent &#x3c3;' with its contraction measure compute guarding context
--
-- @
-- 	gc(D(wi)) = {(P(k), t', v) &#8712; &#x3c0;(wi) | ... }
-- @
guardingContext :: (Eq a, Eq b, Ord c) =>
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
			-- pkt' == pkt'' && 
			isJust (t' `match` t'')
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
loops :: Eq a => RewTree a b c d -> [Loop a b c] 
loops (RTEmpty)	= []
loops (RT _ _ ands) = concat $ concatMap loops0 ands
	where
		loops0 (AndNode _ ors) = concat $ zipWith (oLoops []) [0..] ors

aLoops :: (Eq a) =>
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

oLoops :: (Eq a) =>
	[(Term a b c, (Int, Int))]
	-> Int -- ^ clause ix
	-> OrNode (Clause a b c) (Term a b c) d
	-> [[Loop a b c]]
oLoops _	_	(OrNodeEmpty _) = []
oLoops tws	ci	(OrNode _ ands) = concat $ zipWith (aLoops tws ci) [0..] ands 

getProgramLoops :: (Freshable c, Ord c, Eq b, Eq a) =>
        Program a b c -> [(Int, Int)]
getProgramLoops p = map g $ catMaybes (snd $ gc3withLoops p)
        where g ((_, _, c), ti) = (c, ti)


-- | Guradedness check GC3 according to @Defintion 5.6@ for the whole program
gc3withLoops :: (Freshable c, Ord c, Eq b, Eq a) =>
        Program a b c -> (Bool, [Maybe (Loop a b c, Int)])
gc3withLoops p = (r, ls)
        where list = map (gc3oneWithLoops p) p
              r = all (fst) list
              rewTrees = map (snd) list
              ls = concat $ map (map (unguarded . loops')) rewTrees
              unguarded x = case x of
                            [] -> Nothing
                            _ -> Just (x !! 0)

-- | Guradedness check GC3 according to @Defintion 5.6@ for a single clause
gc3oneWithLoops :: (Freshable c, Ord c, Eq a, Eq b) =>
	Program a b c -> Clause a b c -> (Bool, [RewTree a b c VR])
gc3oneWithLoops p c = gcDerTreeWithLoops [] $ (der p c) 

gcDerTreeWithLoops :: (Eq a, Eq b, Ord c) => [GuardingContext a b c] -> DerTree a b c VR -> (Bool, [RewTree a b c VR])
gcDerTreeWithLoops gcs (DT rt trs) = case gcRewTree rt of
                True    -> (and (map fst list), [rt] ++ concat (map snd list))
                False   -> (False, [rt])
        where list = map (gcTransWithLoops gcs) trs -- [(bool, [RewTree a b c VR])

-- | Check whether a transition id guarded
gcTransWithLoops :: (Eq a, Eq b, Ord c) => [GuardingContext a b c] -> Trans a b c Integer -> (Bool, [RewTree a b c VR])
gcTransWithLoops gcs (Trans p rt _ _ cx dt) = case (not $ null gc) && (gc `elem` gcs) of
		True	-> (True, [])
		False	-> gcDerTreeWithLoops (gc:gcs) dt

	where
		gc = guardingContext p rt cx 

loops' :: Eq a => RewTree a b c d -> [(Loop a b c, Int)] 
loops' (RTEmpty)	= []
loops' (RT _ _ ands) = concat $ concatMap loops0 ands
	where
		loops0 (AndNode _ ors) = concat $ zipWith (oLoops' []) [0..] ors

aLoops' :: (Eq a) =>
	[(Term a b c, (Int, Int))]
	-> Int -- ^ parent clause ix
	-> Int -- ^ term ix
	-> AndNode (Clause a b c) (Term a b c) d
	-> [[(Loop a b c, Int)]]
aLoops' tws pci ti (AndNode t ors) = (concatMap f tws) : 
		(concat $ zipWith (oLoops' ((t, (pci, ti)):tws)) [0..] ors)
	where
		f (t', (pci', _ti')) = if pci' == pci && eqs t t'
			then [((t', t, pci),ti)]
			else []
		eqs (Fun t1 _)	(Fun t2 _)	= t1 == t2
		eqs _		_		= False

oLoops' :: (Eq a) =>
	[(Term a b c, (Int, Int))]
	-> Int -- ^ clause ix
	-> OrNode (Clause a b c) (Term a b c) d
	-> [[(Loop a b c, Int)]]
oLoops' _	_	(OrNodeEmpty _) = []
oLoops' tws	ci	(OrNode _ ands) = concat $ zipWith (aLoops' tws ci) [0..] ands 
