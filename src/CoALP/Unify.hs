{-# LANGUAGE FlexibleContexts #-}
module CoALP.Unify (
	  match
	, unify
	, applySubst
	, composeSubst
) where

import Control.Monad (join)
import Control.Applicative((<*>))
import Data.Functor ((<$>))
import Data.List (sortBy)

import CoALP.Program
import CoALP.FreshVar

import Debug.Trace

term1, term2 :: Term String Integer Int
term1 = Fun "Parent" [Var 1, (Fun "John" [])]
term2 = Fun "Parent" [(Fun "John" []), Var 1]


combineSubst :: (Eq a, Eq b, Ord b) => Subst a b c -> Subst a b c -> Maybe (Subst a b c)
combineSubst s1 s2 = c' (sortBy cmp s1) (sortBy cmp s2)
	where
		cmp a b = fst a `compare` fst b
		c' [] s = Just s
		c' s [] = Just s
		c' (s1'@(h1:t1)) (s2'@(h2:t2)) 
			| fst h1 < fst h2	= fmap (h1:) (c' t1 s2')
			| fst h1 > fst h2	= fmap (h2:) (c' s1' t2)
			| h1 == h2		= fmap (h2:) (c' t1 t2)			
			| otherwise		= Nothing

combineMSubst :: (Ord b, Eq a, Eq b) =>
	Maybe (Subst a b c) -> Maybe (Subst a b c) -> Maybe (Subst a b c)
combineMSubst s1 s2 = join $ combineSubst <$> s1 <*> s2


applySubst :: Subst a b c -> Term a b c -> Term a b c
applySubst [] t = t -- optimize
applySubst s (Fun idnt ts) = Fun idnt $ fmap (applySubst s) ts
applySubst s (Var v) = case lookup v s of
	Just st	-> st
	Nothing	-> Var v

composeSubst :: Subst a b c -> Subst a b c -> Subst a b c
composeSubst s1 s2 = filter neq $ (fmap f s1) ++ s2
	where
		f (v,t) = (v, applySubst s2 t)
		neq (v,Fun _ _) = True
		neq (v,Var v') = v /= v'

renameApart :: (Eq b, Freshable b) => Term a b c -> Term a b c -> (Term a b c, Term a b c)
renameApart t1 t2 = (freshMap apartL t1, freshMap apartR t2)
	where
		freshMap f (Var i) = Var (f i)
		freshMap f (Fun n ts) = Fun n $ fmap (freshMap f) ts

-- TODO proper matching¡
match :: (Ord b, Eq a, Eq b) => Term a b c -> Term a b c -> Maybe (Subst a b c)
match (Var x1) 		(Var x2)	= Just $ (x1, Var x2):[] -- ? should be fresh?
match (Fun id1 ts1)	(Fun id2 ts2)	= if id1 == id2 && length ts1 == length ts2
	then foldr combineMSubst (Just []) (zipWith match ts1 ts2) -- ^ TODO it is neccessary to combine
	-- the partial matches properly, this is buggy
	else Nothing
-- Just $ id1 == id2 && (all (== True) $ zipWith match t1 t2)
--match (Fun _ []) 	(Var _)		= Just True
match (Var x) 		(Fun id1 ts)	= Just $ (x, Fun id1 ts):[]
match _ 		_		= Nothing


unify :: (Ord b, Eq a, Eq b, Show a, Show b, Show c) => Term a b c -> Term a b c -> Maybe (Subst a b c)
unify t1 t2 = traceShow t1 $ traceShow t2 $ unifyImpl [(t1,t2)]


-- | Wiki, yay!
--
-- http://en.wikipedia.org/wiki/Unification_%28computer_science%29#A_unification_algorithm
--
unifyImpl :: [(Term a b c, Term a b c)] -> Maybe (Subst a b c)
unifyImpl [] = Just []
unifyImpl ((t1, t2):ts )
	-- drop constant, coul be actualy droped due to the next rule
	| Fun id1 [] <- t1,
	  Fun id2 [] <- t2,
	  id1 == id2			= unifyImpl ts
	-- decompose
	| Fun id1 ts1 <- t1,
	  Fun id2 ts2 <- t2,
	  id1 == id2,
	  length ts1 == length ts2	= unifyImpl $ ts ++ zip ts1 ts2
	-- conflict
	| Fun id1 _ <- t1,
	  Fun id2 _ <- t2,
	  id1 /= id2			= Nothing
	-- swap
	| Fun id1 ts1 <- t1,
	  Var _ <- t2			= unifyImpl $ (t2,t1):ts
	-- eliminate
	| Var v <- t1,
	  Fun _ _ <- t2			= Just (composeSubst [(v, t2)]) <*> unifyImpl ts
	  --occursCheck v t2		= 
	-- occurscheck fails
	| otherwise			= Nothing



