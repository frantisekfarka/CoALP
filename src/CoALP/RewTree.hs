-- | Module thtat constructs rewriting tree
module CoALP.RewTree (
	rew
) where

import Data.Functor ((<$>))

import Debug.Trace

import CoALP.Program (Program, Clause(..), Subst, RewTree(..),
	AndNode(..),OrNode(..),Term(..),Query(..))

rew :: (Show a, Show b, Show c) =>  Program a b c -> Query a b c -> Subst a b c -> RewTree a b c
rew p c@(Query b) s = RT c s (fmap (mkAndNode p) b)

--makeAnds :: Program a b c -> Term a b c -> [AndNode (Clause a b c)]

mkAndNode ::(Show a, Show b, Show c) =>   Program a b c -> Term a b c -> AndNode (Term a b c) (Clause a b c)
mkAndNode p t = AndNode t $ fmap (mkOrNode p t) p

mkOrNode :: (Show a, Show b, Show c) =>  Program a b c -> Term a b c -> Clause a b c -> OrNode (Clause a b c) (Term a b c)
mkOrNode p t c@(Clause h b)  = case h `match` t of
	Just s	->	let sb = (s `subst`) <$> b in OrNode (Clause t sb) ((mkAndNode p) <$> sb)
	--Just s	->	OrNode (Clause h (([] `subst`) <$> b)) (fmap (mkAndNode p) b)
	Nothing	->	OrNodeEmpty


-- TODO proper matching¡
match :: {-(Show a, Show b, Show c) =>-} Term a b c -> Term a b c -> Maybe (Subst a b c)
match (Var x1) 		(Var x2)	= Just $ (x1, Var x2):[] -- ? should be fresh?
match (Fun id1 t1)	(Fun id2 t2)	= if id1 == id2
	then concat <$> sequence (zipWith match t1 t2)
	else Nothing
-- Just $ id1 == id2 && (all (== True) $ zipWith match t1 t2)
--match (Fun _ []) 	(Var _)		= Just True
match (Var x) 		(Fun id ts)	= Just $ (x, Fun id ts):[]
match _ 		_		= Nothing


subst :: Subst a b c -> Term a b c -> Term a b c
subst s (Var x)		= maybe (Var x) id (x `lookup` s)
subst s (Fun id ts)	= Fun id (subst s <$> ts)
