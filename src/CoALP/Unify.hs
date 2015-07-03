{-# LANGUAGE FlexibleContexts #-}
module CoALP.Unify (
	  match
	, unify
	, applySubst
	, composeSubst
	, unifyImpl
	--, renameApart
	, stripVars
) where

import Control.Arrow((***))
import Control.Monad (join)
import Control.Applicative((<*>))
import Data.Functor ((<$>))
import Data.List (sortBy, nub)

import CoALP.Program
import CoALP.FreshVar (Freshable)

--import Debug.Trace

combineSubst :: (Eq a, Eq b, Ord b) => Subst a b c -> Subst a b c -> Maybe (Subst a b c)
combineSubst s1 s2 = nub <$> c' (sortBy cmp s1) (sortBy cmp s2)
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

composeSubst :: (Eq a, Eq b) => Subst a b c -> Subst a b c -> Subst a b c
composeSubst s1 s2 = nub $ filter neq $ (fmap f s1) ++ s2
	where
		f = id *** (applySubst s2)
		neq (_,Fun _ _) = True
		neq (v,Var v') = v /= v'

--renameApart :: (Eq b, Freshable b) => Term a b c -> Term a b c -> (Term a b c, Term a b c)
--renameApart t1 t2 = (freshMap apartL t1, freshMap apartR t2)
--	where
--		freshMap f (Var i) = Var (f i)
--		freshMap f (Fun n ts) = Fun n $ fmap (freshMap f) ts

-- TODO proper matching¡
--
-- in a sense ``isMatchedTo''
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


-- TODO is this OK?
--
-- it the algorithm we never actualy unify two terms, we always check, whether a
-- term is unifiable with another, and then apply the substitution
--

unify :: (Ord b, Eq a, Eq b, Freshable b) => Term a b c -> Term a b c -> Maybe (Subst a b c)
unify t1 t2 = unifyImpl [(t1,t2)]

-- | Wiki, yay!
--
-- http://en.wikipedia.org/wiki/Unification_%28computer_science%29#A_unification_algorithm
--
unifyImpl :: (Eq a, Eq b) =>
	[(Term a b c, Term a b c)] 
	-> Maybe (Subst a b c)
unifyImpl [] = Just []
unifyImpl ((t1, t2):ts )
	-- delete
	| t1 == t2 			= unifyImpl ts
	-- decompose
	| Fun id1 ts1 <- t1,
	  Fun id2 ts2 <- t2,
	  id1 == id2,
	  length ts1 == length ts2	= unifyImpl $ zip ts1 ts2 ++ ts
	-- conflict
	| Fun id1 ts1 <- t1,
	  Fun id2 ts2 <- t2,
	  (id1 /= id2 ||
	  length ts1 /= length ts2)	= Nothing
	-- swap
	| Fun _ _ <- t1,
	  Var _ <- t2			= unifyImpl $ (t2,t1):ts
	-- eliminate 
	| Var v <- t1,
	  -- Fun _ _ <- t2, 
	  (not $ t1 `subtermOf` t2),
	  t1 `inVars`  ts		= unifyImpl $ let
			s = [(v,t2)] 
			aps = applySubst s
	  	in (t1,t2):(fmap (aps *** aps) ts)
	-- occurscheck 
	| Var _ <- t1,
	  -- Fun _ _ <- t2,
	  (t1 `subtermOf` t2)		= Nothing
	-- equation is already solved
	| Var v <- t1,
	  -- Fun _ _ <- t2,
	  (not $ t1 `subtermOf` t2),
	  (not $ t1 `inVars`  ts)	= Just ((v,t2):) <*> unifyImpl ts
	| otherwise			= error $ "Wtf? Impssible " 
		-- impossible branch

	where
		inVars _ ts' = any (\x -> t1 `subtermOf` (fst x) || t1 `subtermOf` (snd x)) ts'

{-
-- | Separate two terms by renaming apart
apartTerms :: (Eq b, Freshable b) => Term a b c -> Term a b c -> (Term a b c, Term a b c)
apartTerms t1 t2 = (mapVar (apartR) t1, mapVar (apartL) t2)

separateSubst :: (Eq b, Freshable b) => Subst a b c -> (Subst a b c, Subst a b c)
separateSubst s = (map f $ filter (isL . fst) s
	, map f $ filter (isR . fst) s
	)
	where
		f (a, b) = (unpart a, mapVar (apart a) b)
-}


stripVars :: Eq b =>  Subst a b c -> Term a b c -> Subst a b c
stripVars s t = foldr (\x -> filter ((x /=) . fst)) s (vars t)
	where
		vars :: Term a b c -> [b]
		vars (Fun _ ts)	= concatMap vars ts
		vars (Var i)		= [i]



