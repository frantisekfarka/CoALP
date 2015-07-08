-- | Module that transforms logic programs automatically
module CoALP.Transform (
         transformProg
       , transformClause
) where

import CoALP.Program (Program,Program1,Clause1, Clause(..),Term1,Term(..))

transformProg :: (Program1, Integer) -> Program1
transformProg prgWithCount =  transformProgAux prgWithCount 1

transformProgAux :: (Program1, Integer) -> Integer -> Program1
transformProgAux ([], _) _ = []
transformProgAux ((x:xs), count) index = [newClause] ++ transformProgAux (xs, lastVar) (index + 1)
  where transformed = transformClause count index x
        newClause = fst transformed
        lastVar = snd transformed
        
transformClause :: Integer -> Integer -> Clause1 -> (Clause1, Integer)
transformClause count index (Clause h b) = (Clause (transformHead count numOfTerms index h) (transformBody count b), count + numOfTerms)
  where numOfTerms = toInteger . length $ b

transformHead :: Integer -> Integer -> Integer -> Term1 -> Term1
transformHead count n index (Fun ids ts) = Fun ids (ts ++ [newFunc])
  where vars = map (Var) [count..(count + n - 1)]
        newFunc = Fun ("transform-func-" ++ show index) vars

transformBody :: Integer -> [Term1] -> [Term1]
transformBody _ [] = []
transformBody count (t:ts) = [addVar count t] ++ transformBody (count+1) ts

addVar :: Integer -> Term1 -> Term1
addVar var (Fun ids ts) = Fun ids (ts ++ [Var var])


