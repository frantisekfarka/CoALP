{-# LANGUAGE FlexibleContexts #-}
-- | Unification and matching
module CoALP.Unify (
	  match
	, unify
	, applySubst
	, composeSubst
	, stripVars
) where

import Control.Arrow((***))
import Control.Monad (join)
import Control.Applicative((<*>))
import Data.Functor ((<$>))
import Data.List (sortBy, nub)

import CoALP.Program


-- | Combine two substitution when matching
--
combineSubst :: (Eq a, Eq b, Eq c, Ord c) => Subst a b c -> Subst a b c -> Maybe (Subst a b c)
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

-- | Helper combining two substitutions
--
combineMSubst :: (Ord c, Eq a, Eq b, Eq c) =>
	Maybe (Subst a b c) -> Maybe (Subst a b c) -> Maybe (Subst a b c)
combineMSubst s1 s2 = join $ combineSubst <$> s1 <*> s2



-- | Apply substitutuon &#x3b8; to a term t:
--
-- @
-- 	t' = &#x3b8; t
-- @
applySubst :: (Eq c) => Subst a b c -> Term a b c -> Term a b c
applySubst [] t = t -- optimize
applySubst s (Fun idnt ts) = Fun idnt $ fmap (applySubst s) ts
applySubst s (Var v) = case lookup v s of
	Just st	-> st
	Nothing	-> Var v

-- | Compose substitutuons &#x3c3; and &#x3b8;:
--
-- @
-- 	 &#x3c3; (&#x3b8; t) = (&#x3c3; `composeSubst`  &#x3b8;) t
-- @
composeSubst :: (Eq a, Eq b, Eq c) => Subst a b c -> Subst a b c -> Subst a b c
composeSubst s1 s2 = nub $ filter neq $ (fmap f s1) ++ s2'
	where
		f = id *** (applySubst s2)
		neq (_,Fun _ _) = True
		neq (v,Var v') = v /= v'
		s1vs = fmap fst s1
		s2' = filter (\x -> not (fst x `elem` s1vs)) s2

-- | Compute the most general matcher &#x3b8; for
--
-- @
-- 	t1 `match` t2
-- @
--
-- s.t.
--
-- @
-- 	&#x3b8;t1 = t2
-- @
--
match :: (Eq a, Eq b, Eq c, Ord c) => Term a b c -> Term a b c -> Maybe (Subst a b c)
match (Var x1) 		(Var x2)	= Just $ (x1, Var x2):[] 
match (Fun id1 ts1)	(Fun id2 ts2)	= if id1 == id2 && length ts1 == length ts2
	then foldr combineMSubst (Just []) (zipWith match ts1 ts2) 
	else Nothing
match (Var x) 		(Fun id1 ts)	= Just $ (x, Fun id1 ts):[]
match _ 		_		= Nothing


-- | Compute the most general unifier &#x3b8; for
--
-- > t1 `unify` t2
--
-- s.t.
--
-- @
-- 	&#x3b8;t1 = &#x3b8;t2
-- @
--
unify :: (Eq a, Eq c)  => Term a b c -> Term a b c -> Maybe (Subst a b c)
unify t1 t2 = unifyImpl [(t1,t2)]

-- | Implementation of the mgu
--
-- http://en.wikipedia.org/wiki/Unification_%28computer_science%29#A_unification_algorithm
--
unifyImpl :: (Eq a, Eq c) =>
	[(Term a b c, Term a b c)] 
	-> Maybe (Subst a b c)
unifyImpl ts = f <$> unifyImpl' ts 
	where
		f s = rewapply [] s
		rewapply s [] 		= s
		rewapply s (x:xs)	= rewapply (x:(fmap (id *** (applySubst [x])) s)) xs

-- | Ditto
unifyImpl' :: (Eq a, Eq c) =>
	[(Term a b c, Term a b c)] 
	-> Maybe (Subst a b c)
unifyImpl' [] = Just []
unifyImpl' ((t1, t2):ts )
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
	--  equation is already solved
	| Var v <- t1,
	  -- Fun _ _ <- t2,
	  (not $ t1 `subtermOf` t2),
	  (not $ t1 `inVars`  ts)	= Just ((v,t2):) <*> unifyImpl ts
	| otherwise			= error $ "Wtf? Impssible " 
		-- impossible branch

	where
		inVars _ ts' = any (\x -> t1 `subtermOf` (fst x) || t1 `subtermOf` (snd x)) ts'

-- | Strip substitution of variables in the term
--
-- the term is usualy the head of a clause
stripVars :: Eq c =>  Subst a b c -> Term a b c -> Subst a b c
stripVars s t = foldr (\x -> filter ((x /=) . fst)) s (vars t)
	where
		--vars :: Term a b c -> [b]
		vars (Fun _ ts)	= concatMap vars ts
		vars (Var i)		= [i]



