{-# LANGUAGE FlexibleContexts #-}
module CoALP.Unify (
	  match
	, unify
	, applySubst
	, composeSubst
	, unifyImpl
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

composeSubst :: Eq b => Subst a b c -> Subst a b c -> Subst a b c
composeSubst s1 s2 = prune $ filter neq $ (fmap f s1) ++ s2
	where
		f (v,t) = (v, applySubst s2 t)
		neq (v,Fun _ _) = True
		neq (v,Var v') = v /= v'
		prune [] = []
		prune (x:xs) = x:(prune (pruneEl x xs))
		pruneEl (v, _) [] = []
		pruneEl v (w:ws) = if fst v == fst w then ws else v:(pruneEl v ws)

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


unify :: (Ord b, Eq a, Eq b, Freshable b, Show a, Show b, Show c) => Term a b c -> Term a b c -> Maybe (Subst a b c)
--unify t1 t2 = traceShowId $ fmap (snd . separateSubst) $ traceShowId $ unifyImpl $ traceShowId [apartTerms t1 t2]
unify t1 t2 = traceShowId $ fmap snd $ traceShowId $ fmap separateSubst $ traceShowId $ collapseS $ unifyImpl $ traceShowId [apartTerms t1 t2]
	where
		-- TODO fix 99 :-)
		collapseS (Just x) = Just $ foldr f x (take 99 (repeat x))
		collapseS x = x
		f x y = g $ composeSubst x y
		g [] = []
		g (x:xs) = x:(g (elim x xs))
		elim _ [] = []
		elim x (y:ys) = if (x == y) then ys else y:(elim x ys)


-- | Wiki, yay!
--
-- http://en.wikipedia.org/wiki/Unification_%28computer_science%29#A_unification_algorithm
--
unifyImpl :: (Eq b, Freshable b, Show a, Show b, Show c) => [(Term a b c, Term a b c)] -> Maybe (Subst a b c)
unifyImpl [] = Just []
unifyImpl ((t1, t2):ts )
	-- bind vars
	| Var v1 <- t1,
	  Var v2 <- t2			= Just (composeSubst ( 
	  					composeSubst [(v1, Var $ apart v1 v2)]
							[(v2, Var $ apart v1 v2)])
					) <*> unifyImpl ts
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
	  id1 /= id2			= trace ("Failed to unify " ++ show (t1, t2)) Nothing
	-- swap
	| Fun id1 ts1 <- t1,
	  Var _ <- t2			= unifyImpl $ (t2,t1):ts
	-- eliminate
	| Var v <- t1,
	  Fun _ _ <- t2			= trace (
	  		"PartSubst: " ++ show v ++ " (" ++ show (unpart v) ++ "):\t" ++ show t2 ++ "\twas:\t" ++ show (mapVar unpart t2) ++"\n"
	  	) Just (composeSubst [(v, t2)]) <*> unifyImpl ts
	  --occursCheck v t2		= 
	-- occurscheck fails
	| _ <- t1, _ <- t2, 
		otherwise			= traceShow (t1, t2) Nothing


-- | Separate two terms by renaming apart
apartTerms :: (Eq b, Freshable b) => Term a b c -> Term a b c -> (Term a b c, Term a b c)
apartTerms t1 t2 = (mapVar (apartR) t1, mapVar (apartL) t2)

separateSubst :: (Eq b, Freshable b) => Subst a b c -> (Subst a b c, Subst a b c)
separateSubst s = (map f $ filter (isL . fst) s
	, map f $ filter (isR . fst) s
	)
	where
		f (a, b) = (unpart a, mapVar (apart a) b)



