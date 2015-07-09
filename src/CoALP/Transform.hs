-- | Module that transforms logic programs automatically
module CoALP.Transform (
         transformProg
       , transformClause
       , transformHead
       , transformBody
       , addVar
) where

import CoALP.Program (Program,Program1,Clause1, Clause(..),Term1,Term(..))

-- Transform a program
-- prgWithCount - pair with the program to transform and the next fresh variable
transformProg :: (Program1, Integer) -> Program1
transformProg prgWithCount =  transformProgAux prgWithCount 1

transformProgAux :: (Program1, Integer) -> Integer -> Program1
transformProgAux ([], _) _ = []
transformProgAux ((x:xs), count) index = [newClause] ++ transformProgAux (xs, lastVar) (index + 1)
  where transformed = transformClause count index x
        newClause = fst transformed
        lastVar = snd transformed

-- Transforms a clause
-- count - current fresh variable
-- index - index of the clause in the program
transformClause :: Integer -> Integer -> Clause1 -> (Clause1, Integer)
transformClause count index (Clause h b) = (Clause (transformHead count numOfTerms index h) (transformBody count b), count + numOfTerms)
  where numOfTerms = toInteger . length $ b

-- Adds a function containing the variables used to the head of the clause
-- count - first fresh variable
-- n - number of fresh vars in the body
-- index - index of the clause in the program
transformHead :: Integer -> Integer -> Integer -> Term1 -> Term1
transformHead count n index (Fun ids ts) = Fun ids (ts ++ [newFunc])
  where vars = map (Var) [count..(count + n - 1)]
        newFunc = Fun ("transform-func-" ++ show index) vars

-- Adds variables to the body of a clause
-- count - current fresh variable
transformBody :: Integer -> [Term1] -> [Term1]
transformBody _ [] = []
transformBody count (t:ts) = [addVar count t] ++ transformBody (count+1) ts

-- Adds a variable to the term
-- var - variable number to add
addVar :: Integer -> Term1 -> Term1
addVar var (Fun ids ts) = Fun ids (ts ++ [Var var])


