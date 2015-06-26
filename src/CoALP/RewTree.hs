{-# LANGUAGE FlexibleContexts #-}
-- | Module that constructs rewriting tree
module CoALP.RewTree (
	    rew
	  , getVrs
) where

import Data.Functor ((<$>))
import Data.Traversable (sequenceA)

import CoALP.FreshVar (FreshVar,getFresh,evalFresh,Freshable(..),initFresh)
import CoALP.Unify (match, applySubst, composeSubst, stripFreeVars)
import CoALP.Program (Program, Clause(..), Subst, RewTree(..),
	AndNode(..),OrNode(..),Term(..),Vr(..),
	mapTerm,mapClause,mapSubst
	)


--rew :: (Eq a, Eq b, Ord b, Freshable b, Freshable d) =>
--	Program a b c -> Clause a b c -> Subst a b c -> RewTree a b c d
rew p c@(Clause h b) si = flip evalFresh initFresh $ do
		ands <- sequenceA $ fmap (mkAndNode p si) bsi'
		return $ RT csi' si ands
			--(Clause h' b') s' ands
	where
		c = Clause h b
		si' = si `stripFreeVars` c
		h' = h -- free vars are stripped, no need to apply si'
		bsi' = map (si' `applySubst`) b
		csi' = Clause h' bsi'

		{-p = map (mapClause f) p0
		b = fmap (mapTerm (f)) b0
		h = mapTerm apartR h0
		si = mapSubst f si0
		f = id
		-}

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
mkOrNode p si t c@(Clause h b)  = case (h `match` t) of
		Just th ->	do
			let	thb = map (applySubst th) b
			let	si' = si `stripFreeVars` (Clause t thb)
			let	sithb' = map (applySubst si') thb
			ands <- sequenceA ((mkAndNode p si) <$> sithb')
			return $ OrNode (Clause t sithb') ands
		Nothing	->	getFresh >>= return . OrNodeEmpty . Vr

	where


-- | apply substitution to the term tree
subst :: Subst a b c -> Term a b c -> Term a b c
subst s (Var x)		= maybe (Var x) id (x `lookup` s)
subst s (Fun idnt ts)	= Fun idnt (subst s <$> ts)


-- get Vr variables in rew tree
-- TODO breath first search - thus we can show finite subtrees of infinitely
-- branching DerTrees
getVrs :: RewTree a b c d -> [Vr d]
getVrs RTEmpty = []
getVrs (RT _ _ ands) = concatMap getAndVrs ands
	where 
		getAndVrs (AndNode _ os) = concatMap getOrVrs os
		getOrVrs (OrNodeEmpty c) = [c]
		getOrVrs (OrNode _ as) = concatMap getAndVrs as

