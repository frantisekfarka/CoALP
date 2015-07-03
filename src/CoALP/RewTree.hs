{-# LANGUAGE FlexibleContexts #-}
-- | Module that constructs rewriting tree
module CoALP.RewTree (
	    rew
	  , getVrs
	  , loops
) where

import Control.Arrow ((***))
import Data.Functor ((<$>))
import Data.Traversable (sequenceA,traverse)

import CoALP.FreshVar (FreshVar,getFresh,evalFresh,Freshable(..),initFresh)
import CoALP.Unify (match, applySubst, stripVars)
import CoALP.Program (Program, Clause(..), Subst, RewTree(..),
	AndNode(..),OrNode(..),Term(..),Vr(..),
	--mapTerm,mapClause,mapSubst
	)


rew :: (Eq a, Ord b, Freshable b, Freshable d) =>
	Program a b c -> Clause a b c -> Subst a b c -> RewTree a b c d
rew p (Clause h b) si = flip evalFresh initFresh $ do
		ands <- sequenceA $ fmap (mkAndNode p si) bsi'
		return $ RT csi' si ands
			--(Clause h' b') s' ands
	where
		si' = si `stripVars` h 
		h' = si' `applySubst` h -- free vars are stripped, no need to apply si'
		bsi' = map (si' `applySubst`) b
		csi' = Clause h' bsi'

-- | AndNode aka Term node
mkAndNode :: (Eq a, Eq b, Ord b, Freshable b, Freshable d) => 
	Program a b c -> Subst a b c -> Term a b c -> 
	FreshVar d (AndNode (Clause a b c) (Term a b c) (Vr d))
mkAndNode p si' t = do
	ors <- sequenceA $ fmap (mkOrNode p si' t) p
	return $ AndNode t ors

-- | OrNode aka Clause node
mkOrNode :: (Eq a, Eq b, Ord b, Freshable b, Freshable d) =>
	Program a b c -> Subst a b c -> Term a b c -> Clause a b c -> 
	FreshVar d (OrNode (Clause a b c) (Term a b c) (Vr d))
mkOrNode p si t (Clause h b)  = case (h `match` t) of
		Just th ->	do
			let	thb = map (applySubst th) b
			let	thh = applySubst th h
			let	si' = si `stripVars` thh
			let	sithb' = map (applySubst si') thb
			ands <- sequenceA ((mkAndNode p si) <$> sithb')
			return $ OrNode (Clause t sithb') ands
		Nothing	->	getFresh >>= return . OrNodeEmpty . Vr

	where


-- | apply substitution to the term tree
--subst :: Subst a b c -> Term a b c -> Term a b c
--subst s (Var x)		= maybe (Var x) id (x `lookup` s)
--subst s (Fun idnt ts)	= Fun idnt (subst s <$> ts)


-- | Get Transiotion Variables in breath-first search manner
--
-- for GC3 is fine to proceed in DFS, however for displaying the search
-- up-to-depth n the BFS is necessary
--
getVrs :: RewTree a b c d -> [(Vr d, Term a b c, Int)]
getVrs RTEmpty 		= []
getVrs (RT _ _ ands) 	= concatMap processAnd ands
	where
		processAnd (AndNode t ors) 		= 
			(concat $ zipWith (processOr t) ors [0..])
			++ concatMap continueOr ors
		processOr _ (OrNode _ _) 	_ 	= []
		--processOr _ (OrNode _ ands') 	_ 	= concatMap processAnd ands'
		processOr t (OrNodeEmpty d)	pi	= [(d, t, pi)]
		continueOr (OrNode _ ands')		= concatMap processAnd ands'
		continueOr _ 				= []


-- TODO Freshable!
-- TODO
-- 	rewrite, I believe that the following is better:
--
-- 	go from the top, accumulate (clause head, origin in P)
-- 	when encountered new clause, compare with head, if forms loop, 
-- 	push loop on loop stack, push (head, origin) on accumulator
loops :: (Freshable d) =>
	RewTree a b c d -> [(Term a b c, Term a b c, Int)]
loops rt = snd (loops' rt)

-- | recursively build loops
loops' :: Freshable d =>
	RewTree a b c d -> ([(Term a b c,Int)],[(Term a b c, Term a b c, Int)])
loops' RTEmpty = ([],[])
loops' (RT _ _ ands) = (id *** concat.concat) $ sequenceA $ fmap f ands
	where
		f (AndNode _ ors) = sequenceA $ zipWith loopsO [0..] ors

loopsA :: Int -> AndNode (Clause a b c) (Term a b c) d 
	-> ([(Term a b c, Int)],[(Term a b c, Term a b c, Int)])
loopsA pari (AndNode f@(Fun fid _) ors) = (id *** concat) $
				sequenceA $ ([(f,pari)],newLoops) : boundLower
	where
		boundLower = zipWith loopsO [0..] ors
		newLoops = [(f, f', pari) | 
			(f'@(Fun fid' _), pari')  <- concatMap fst boundLower,
			fid == fid' && pari == pari'
			]
loopsA _ (AndNode (Var _) ors) = (id *** concat) $ sequenceA $ zipWith loopsO [0..] ors

loopsO :: Int -> OrNode (Clause a b c) (Term a b c) d
	-> ([(Term a b c, Int)],[(Term a b c, Term a b c, Int)])
loopsO _ (OrNodeEmpty _) = ([],[])
loopsO pari (OrNode _  ands) = (id *** concat) $ traverse (loopsA pari) ands


