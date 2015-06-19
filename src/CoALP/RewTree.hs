{-# LANGUAGE FlexibleContexts #-}
-- | Module that constructs rewriting tree
module CoALP.RewTree (
	    rew
	  , getVrs
) where

import Data.Functor ((<$>))
import Data.Traversable (sequenceA)

import CoALP.FreshVar (FreshVar,getFresh,evalFresh,Freshable(..),initFresh)
import CoALP.Unify (match, unify, applySubst, composeSubst)
import CoALP.Program (Program, Clause(..), Subst, RewTree(..),
	AndNode(..),OrNode(..),Term(..),Query(..),Vr(..),mkVar
	)

rew :: (Eq a, Eq b, Ord b, Freshable b, Freshable d) =>
	Program a b c -> Query a b c -> Subst a b c -> RewTree a b c d
rew p c@(Query b) s = flip evalFresh initFresh $ do
		ands <- sequenceA $ fmap (mkAndNode p s) b'
		return $ RT c s ands
	where
		b' = fmap (applySubst s) b

-- | AndNode aka Term node
mkAndNode :: (Eq a, Eq b, Ord b, Freshable b, Freshable d) => 
	Program a b c -> Subst a b c -> Term a b c -> FreshVar d (AndNode (Clause a b c) (Term a b c) (Vr d))
mkAndNode p os t = do
	ors <- sequenceA $ fmap (mkOrNode p os t) p
	return $ AndNode t ors

-- | OrNode aka Clause node
mkOrNode :: (Eq a, Eq b, Ord b, Freshable b, Freshable d) =>
	Program a b c -> Subst a b c -> Term a b c -> Clause a b c -> FreshVar d (OrNode (Clause a b c) (Term a b c) (Vr d))
mkOrNode p os t (Clause h b)  = case h' `match` t' of
	Just s	->	do
				ands <- sequenceA ((mkAndNode p os) <$> (sb' s))
				return $ OrNode (Clause t (sb' s)) ands
	Nothing	->	getFresh >>= return . OrNodeEmpty . Vr

	where
		sb' s' = map (mapTerm unpart) $ ((os `composeSubst` s') `subst`) <$> b'
		h' = mapTerm apartL h
		b' = map (mapTerm apartL) b
		t' = mapTerm apartR t

-- TODO make Term (bi)functor
mapTerm :: (Eq b, Eq b') => (b -> b') -> Term a b c -> Term a b' c
mapTerm f (Fun idn ts) = Fun idn $ map (mapTerm f) ts
mapTerm f (Var a) = Var $ f a


-- | apply substitution to the term tree
subst :: Subst a b c -> Term a b c -> Term a b c
subst s (Var x)		= maybe (Var x) id (x `lookup` s)
subst s (Fun idnt ts)	= Fun idnt (subst s <$> ts)


-- get Vr variables in rew tree
-- TODO breath first search - thus we can show finite subtrees of infinitely
-- branching DerTrees
getVrs :: RewTree a b c d -> [d]
getVrs RTEmpty = []
getVrs (RT _ _ ands) = concatMap getAndVrs ands
	where 
		getAndVrs (AndNode _ os) = concatMap getOrVrs os
		getOrVrs (OrNodeEmpty (Vr c)) = [c]
		getOrVrs (OrNode _ as) = concatMap getAndVrs as

