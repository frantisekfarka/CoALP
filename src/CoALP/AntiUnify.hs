
-- | 
-- Module that performs anti-unification
module CoALP.AntiUnify (
  antiUnify
  , antiUnifyClause
  , substMapInit
  , antiUnifyWithCount
) where

import Control.Monad.Trans.State (StateT, get, put, runStateT) 
import Control.Monad.Identity
import CoALP.FreshVar (getFresh,evalFresh,Freshable(..),initFresh)
import CoALP.Program (Term(..), Clause(..))
-- | Substitution on terms
--type Subst a b c = [(c, Term a b c)]


type AntiUnifySt a b c d = StateT (SubstMap a b c) Identity d
type SubstAU a b c = [((Term a b c, Term a b c), c)]

data SubstMap a b c = SubstMap {
          substitutions :: SubstAU a b c
        --, antiUnifier   :: Maybe (Term a b c)
        , varCount      :: c
        }

-- | Initialize substitution map
--
substMapInit :: (Eq a, Eq b, Eq c, Freshable c) => SubstMap a b c
substMapInit = SubstMap {
          substitutions = [] 
        --, antiUnifier   = Nothing 
        , varCount      = initFresh 
        }

-- | Anti-Unify clauses with two terms
--
antiUnifyClause :: (Eq a, Eq b, Eq c, Freshable c) => Clause a b c ->  Either String (Term a b c, SubstAU a b c)
antiUnifyClause (Clause _ b)
  | length b == 2 = Right (antiUnifyWithSubst (b !! 0) (b !! 1))
  | otherwise     = Left "There are too many or too few terms in the clause body. Only use 2 terms for anti-unification.\n"


-- | Anti-unify and count number of assigne fresh variables
--
antiUnifyWithCount :: (Eq a, Eq b, Eq c, Freshable c) => Term a b c -> Term a b c -> c -> Term a b c
antiUnifyWithCount t1 t2 c = fst $ runIdentity (runStateT (antiUnifyTerms t1 t2) initSubstMap)
  where initSubstMap = substMapInit { varCount = c}

-- | Anti-unify two terms
--
antiUnify :: (Eq a, Eq b, Eq c, Freshable c) => Term a b c -> Term a b c -> Term a b c
antiUnify t1 t2 = fst $ runIdentity (runStateT (antiUnifyTerms t1 t2) substMapInit)

-- | Anti-unify and return also substitution
--
antiUnifyWithSubst :: (Eq a, Eq b, Eq c, Freshable c) => Term a b c -> Term a b c -> (Term a b c, SubstAU a b c)
antiUnifyWithSubst t1 t2 = (res, substitutions s)
  where (res, s) = runIdentity (runStateT (antiUnifyTerms t1 t2) substMapInit)

-- | Anti-unify two terms
--
antiUnifyTerms :: (Eq a, Eq b, Eq c, Freshable c) => Term a b c -> Term a b c -> AntiUnifySt a b c (Term a b c)
antiUnifyTerms t1 t2
  | t1 == t2     = return t1
antiUnifyTerms t1@(Fun idx1 ts1) t2@(Fun idx2 ts2)
  | idx1 == idx2 = do
                   ts <- antiUnifyAux ts1 ts2
                   return (Fun idx1 ts)
  | otherwise    = do
                   x <- substExists (t1,t2)
                   return (Var x)
antiUnifyTerms t1@(Var x1) t2@(Var x2)
  | x1 == x2    = return t1
  | otherwise   = do
                  x <- substExists (t1, t2)
                  return (Var x)
antiUnifyTerms t1@(Var _) t2@(Fun _ _) = do
                  x <- substExists (t1, t2)
                  return (Var x)
antiUnifyTerms t1@(Fun _ _) t2@(Var _) = do
                  x <- substExists (t1, t2)
                  return (Var x)
                  

-- | auxiliary function
--
antiUnifyAux :: (Eq a, Eq b, Eq c, Freshable c) => [Term a b c] -> [Term a b c] -> AntiUnifySt a b c [Term a b c]
antiUnifyAux [] _  = return []
antiUnifyAux _  [] = return []
antiUnifyAux (t1:ts1) (t2:ts2) = do
                                  x <- antiUnifyTerms t1 t2
                                  xs <- antiUnifyAux ts1 ts2
                                  return ([x] ++ xs)

-- | Does a substitution exist?
--
substExists :: (Eq a, Eq b, Eq c, Freshable c) => (Term a b c, Term a b c) -> AntiUnifySt a b c c
substExists ts = do
                 s <- get
                 let subs = substitutions s
                     count = varCount s
                     fresh = evalFresh getFresh count 
                 case containsSub ts subs of
                      True  -> return (lookupSub ts subs)
                      False -> do let freshSub@(_, newVar) = (ts, fresh)
                                  put $ s {substitutions = subs ++ [freshSub],  varCount = newVar}
                                  return newVar
                            

containsSub :: (Eq a, Eq b, Eq c) => (Term a b c, Term a b c) -> SubstAU a b c -> Bool
containsSub _ [] = False
containsSub tp (t:ts)
  | tp == fst t = True
  | otherwise   = containsSub tp ts 

lookupSub :: (Eq a, Eq b, Eq c) => (Term a b c, Term a b c) -> SubstAU a b c -> c
lookupSub _ [] = undefined
lookupSub tp (t:ts)
  | tp == fst t = snd t
  | otherwise   = lookupSub tp ts
