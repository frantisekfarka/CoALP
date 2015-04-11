-- | Module thtat constructs rewriting tree
module CoALP.RewTree (
	rew
) where

import CoALP.Program (Program, Clause(..), Subst, RewTree(..),
	AndNode(..),OrNode(..),Term(..))

rew :: Program a b c -> Clause a b c -> Subst a b c -> RewTree a b c
rew p c@(QueryClause b) s = RT c s (fmap (mkAndNode p) b)
rew p c@(Clause h b) s = RT c s (fmap (mkAndNode p) b)

--makeAnds :: Program a b c -> Term a b c -> [AndNode (Clause a b c)]

mkAndNode :: Program a b c -> Term a b c -> AndNode (Term a b c) (Clause a b c)
mkAndNode p t = AndNode t $ fmap (mkOrNode p t) p

mkOrNode :: Program a b c -> Term a b c -> Clause a b c -> OrNode (Clause a b c) (Term a b c)
mkOrNode p t c@(QueryClause b)  = OrNodeEmpty
mkOrNode p t c@(Clause h b)  = if h `match` t 
	then OrNode c (fmap (mkAndNode p) b)
	else OrNodeEmpty


-- TODO proper matching¡
match :: Term a b c -> Term a b c -> Bool	
match (Var _) 		(Var _)		= True
match (Fun id1 _)	(Fun id2 _)	= id1 == id2
mathc _ 		_		= False


--mkOrNode p c = OrNode t (fmap (mkAndNode p) p)
