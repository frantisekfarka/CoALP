-- | Module thtat constructs rewriting tree
module CoALP.RewTree (
	rew
) where

import CoALP.Program (Program, Clause(..), Subst, RewTree(..),
	AndNode(..),OrNode(..),Term(..),Query(..))

rew :: Program a b c -> Query a b c -> Subst a b c -> RewTree a b c
rew p c@(Query b) s = RT c s (fmap (mkAndNode p) b)

--makeAnds :: Program a b c -> Term a b c -> [AndNode (Clause a b c)]

mkAndNode :: Program a b c -> Term a b c -> AndNode (Term a b c) (Clause a b c)
mkAndNode p t = AndNode t $ fmap (mkOrNode p t) p

mkOrNode :: Program a b c -> Term a b c -> Clause a b c -> OrNode (Clause a b c) (Term a b c)
mkOrNode p t c@(Clause h b)  = if h `match` t 
	then OrNode c (fmap (mkAndNode p) b)
	else OrNodeEmpty


-- TODO proper matching¡
match :: Term a b c -> Term a b c -> Bool	
match (Var _) 		(Var _)		= True
match (Fun id1 t1)	(Fun id2 t2)	= id1 == id2 && (all (== True) $ zipWith match t1 t2)
match (Fun _ []) 	(Var _)		= True
match (Var _) 		(Fun _ _)	= True
match _ 		_		= False


--mkOrNode p c = OrNode t (fmap (mkAndNode p) p)
