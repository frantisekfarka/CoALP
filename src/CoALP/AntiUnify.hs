-- | Module that performs anti-unification

module CoALP.AntiUnify (
  antiUnify
  , substMapInit
  , antiUnifyWithCount
) where

import Control.Monad.Trans.State (StateT, get, put, runStateT) 
import Control.Monad.Identity
--import CoALP.FreshVar (FreshVar,getFresh,evalFresh,Freshable(..),initFresh)
import CoALP.Program (Term(..), Term1)
-- | Substitution on terms
--type Subst a b c = [(c, Term a b c)]


type AntiUnifySt a = StateT SubstMap Identity a
type SubstAU = [((Term1, Term1), Integer)]

data SubstMap = SubstMap {
          substitutions :: SubstAU
        , antiUnifier   :: Maybe Term1
        , varCount      :: Integer
        } deriving (Show, Eq)

substMapInit :: SubstMap
substMapInit = SubstMap {
          substitutions = []
        , antiUnifier   = Nothing
        , varCount      = 0
        }

antiUnifyWithCount :: Term1 -> Term1 -> Integer -> Term1
antiUnifyWithCount t1 t2 c = fst $ runIdentity (runStateT (antiUnifyTerms t1 t2) initSubstMap)
  where initSubstMap = substMapInit { varCount = c }

antiUnify :: Term1 -> Term1 -> Term1
antiUnify t1 t2 = fst $ runIdentity (runStateT (antiUnifyTerms t1 t2) substMapInit)

antiUnifyTerms :: Term1 -> Term1 -> AntiUnifySt Term1
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
                  


antiUnifyAux :: [Term1] -> [Term1] -> AntiUnifySt [Term1]
antiUnifyAux [] _  = return []
antiUnifyAux _  [] = return []
antiUnifyAux (t1:ts1) (t2:ts2) = do
                                  x <- antiUnifyTerms t1 t2
                                  xs <- antiUnifyAux ts1 ts2
                                  return ([x] ++ xs)

substExists :: (Term1, Term1) -> AntiUnifySt Integer
substExists ts = do
                 s <- get
                 let subs = substitutions s
                     count = varCount s
                 case containsSub ts subs of
                      True  -> return (lookupSub ts subs)
                      False -> do let freshSub@(_, newVar) = (ts, count + 1)
                                  put $ s {substitutions = subs ++ [freshSub],  varCount = newVar}
                                  return newVar
                            

containsSub :: (Term1, Term1) -> SubstAU -> Bool
containsSub _ [] = False
containsSub tp (t:ts)
  | tp == fst t = True
  | otherwise   = containsSub tp ts 

lookupSub :: (Term1, Term1) -> SubstAU -> Integer
lookupSub _ [] = 0
lookupSub tp (t:ts)
  | tp == fst t = snd t
  | otherwise   = lookupSub tp ts
