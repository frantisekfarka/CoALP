-- | Term reductions
module CoALP.Reductions (
	 isVarReductOf
	,nvPropSub
) where 

import Control.Arrow ((***))
import Data.Maybe (isJust)

import CoALP.Program(Term(..))



-- | Compute term reduction measure according to @Definition 5.2@
isVarReductOf :: Eq a => Term a b c -> Term a b c -> Maybe (Term a b c)
isVarReductOf (Fun id1 ts1) (Fun id2 ts2) 
	| id1 == id2	= h (filter isJust $ zipWith isVarReductOf ts1 ts2)
	| otherwise	= Nothing
	where
		h [] 	= Nothing
		h (x:_)	= x
isVarReductOf (Var _) mr@(Fun _ _) = Just mr
isVarReductOf (Var _) (Var _) = Nothing
isVarReductOf (Fun _ _) (Var _) = Nothing

-- | Compute non-variable proper subterms of a term
--
-- required for the computation of a guarding context
--
nvPropSub :: Term a b c -> [(Term a b c, [Int])]
nvPropSub t = map (id *** reverse) (nvPropSub_ (t, [0]))

-- | Actual implementation
nvPropSub_ :: (Term a b c, [Int]) -> [(Term a b c, [Int])]
nvPropSub_ ((Fun _ ts),v)	= nvs ++ concatMap nvPropSub_ nvs
	where
		nvs = [(t, v':v) | (t@(Fun _ _), v') <- zip ts [0..]]
nvPropSub_ ((Var _),_)		= []




