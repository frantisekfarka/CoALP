{-# LANGUAGE FlexibleContexts #-}
-- | Construction of a rewriting tree
module CoALP.RewTree (
	    rew
	  , extrew
	  , getVrs
	  , loops
) where

--import Control.DeepSeq (deepseq, force)

import Control.Arrow ((***))
import Data.Functor ((<$>))
import Data.Traversable (sequenceA,traverse)

import CoALP.FreshVar (FreshVar,getFresh,evalFresh,Freshable(..),initFresh)
import CoALP.Unify (match, applySubst, stripVars, composeSubst)
import CoALP.Program (Program, Clause(..), Subst, RewTree(..),
	AndNode(..),OrNode(..),Term(..),Vr(..)
	)

-- | Construct a rewriting tree given program P, clause C and substitution &#x3c3;
--
-- @
-- 	rew (P, C, &#x3c3;)
-- @
--
-- according to @Definition 3.3@
--
rew :: (Eq a, Eq b, Eq c, Ord c, Freshable c, Freshable d) =>
	Program a b c
	-> Clause a b c
	-> Subst a b c
	-> RewTree a b c d
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
mkAndNode :: (Eq a, Eq b, Eq c, Ord c, Freshable c, Freshable d) => 
	Program a b c -> Subst a b c -> Term a b c -> 
	FreshVar d (AndNode (Clause a b c) (Term a b c) (Vr d))
mkAndNode p si' t = do
	ors <- sequenceA $ fmap (mkOrNode p si' t) p
	return $ AndNode t ors

-- | OrNode aka Clause node
mkOrNode :: (Eq a, Eq b, Eq c, Ord c, Freshable c, Freshable d) =>
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
-- | Given rewriting tree rew(P, C, &#x3c3;) and X = T(wi) ∈ V_R construct new
-- rewriting tree T_X accroding to @Definition 3.5@
-- 
--
extrew :: (Eq a, Eq b, Eq c, Ord c, Freshable c, Freshable d) =>
	Program a b c -> RewTree a b c d -> Subst a b c -> RewTree a b c d
extrew _ rt@RTEmpty 	_ 	= rt
extrew p rt		si'	= RT c sisi' (fmap (extAndNode p sisi') ands)
		where
			(RT c sisi' ands) = substRT si' rt


-- | Extend and node
extAndNode :: (Freshable d, Freshable c, Ord c, Eq a, Eq b) =>
	Program a b c
	-> Subst a b c
	-> AndNode (Clause a b c) (Term a b c) (Vr d)
	-> AndNode (Clause a b c) (Term a b c) (Vr d)
extAndNode p sisi' (AndNode t ors) 	= AndNode t $ zipWith (extOrNode p sisi' t) ors p

-- | Extend or node
extOrNode :: (Freshable d, Freshable c, Ord c, Eq a, Eq b) =>
	Program a b c
	-> Subst a b c
	-> Term a b c
	-> OrNode (Clause a b c) (Term a b c) (Vr d)
	-> Clause a b c
	-> OrNode (Clause a b c) (Term a b c) (Vr d)
extOrNode p sisi' _ (OrNode c ands) _	= OrNode c $ fmap (extAndNode p sisi') ands
extOrNode p sisi' t ornd@(OrNodeEmpty (Vr v))  c@(Clause h _) 	= case (h `match` t) of
		Just _	-> flip evalFresh v $ mkOrNode p sisi' t c
		Nothing	-> ornd

-- | Apply substitution over RewritingTree
--
-- TODO - replace by bifunctor ~ bimap + apply
--
substRT :: (Eq a, Eq b, Eq c) =>
	Subst a b c -> RewTree a b c d -> RewTree a b c d
substRT _ 	rt@(RTEmpty)	= rt
substRT th	(RT c si ands)	= RT thc thsi thands
	where
		thc = th `asc` c
		asc rho (Clause h b) = Clause (rho `applySubst` h)
			(fmap (rho `applySubst`) b)
		thsi = th `composeSubst` si
		thands = fmap (substAnd th) ands
		substAnd rho (AndNode t ors) 	= AndNode (rho `applySubst` t)
			(fmap (substOr th) ors)
		substOr _ on@(OrNodeEmpty _)	= on
		substOr rho (OrNode c' ands')	= OrNode (rho `asc` c')
			(fmap (substAnd rho) ands')


-- | Get Transition Variables in breath-first search manner
--
-- for GC3 is fine to proceed in DFS, however for displaying the search
-- up-to-depth n the BFS is necessary
--
getVrs :: RewTree a b c d -> [(Vr d, Term a b c, Int)]
getVrs RTEmpty 		= []
getVrs (RT _ _ ands) 	= (concatMap processAnd ands)
	where
		processAnd (AndNode t ors) 		= 
			(concat $ zipWith (processOr t) ors [0..])
			++ concatMap continueOr ors
		processOr _ (OrNode _ _) 	_ 	= []
		--processOr _ (OrNode _ ands') 	_ 	= concatMap processAnd ands'
		processOr t (OrNodeEmpty d)	pk	= [(d, t, pk)]
		continueOr (OrNode _ ands')		= concatMap processAnd ands'
		continueOr _ 				= []

-- | Given rewriting tree compute loops as described in the 
-- @Definition 5.2@
loops :: (Eq a, Freshable d) =>
	RewTree a b c d -> [(Term a b c, Term a b c, Int)]
loops rt = snd (loops' rt)

-- | Recursively build loops
loops' :: (Eq a, Freshable d) =>
	RewTree a b c d -> ([(Term a b c,Int)],[(Term a b c, Term a b c, Int)])
loops' RTEmpty = ([],[])
loops' (RT _ _ ands) = (id *** concat.concat) $ sequenceA $ fmap f ands
	where
		f (AndNode _ ors) = sequenceA $ zipWith loopsO [0..] ors

-- | Loops in AndNode
loopsA :: Eq a => Int -> AndNode (Clause a b c) (Term a b c) d 
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

-- | Loops in OrNode
loopsO :: Eq a => Int -> OrNode (Clause a b c) (Term a b c) d
	-> ([(Term a b c, Int)],[(Term a b c, Term a b c, Int)])
loopsO _ (OrNodeEmpty _) = ([],[])
loopsO pari (OrNode _  ands) = (id *** concat) $ traverse (loopsA pari) ands


