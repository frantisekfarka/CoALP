-- | Module that transforms logic programs automatically
module CoALP.Transform (
         transformProg
       , transformClause
       , transformHead
       , transformBody
       , addVar
       , annotateProg
       , annotateClause
       , annotateTransFunc
       , annotateBody
       , annotateTerm
       , annotateLast
) where

import CoALP.Program (Program1,Clause1, Clause(..),Term1,Term(..))
--import CoALP.Guards


annotateProg :: Program1 -> [(Int, Int)] -> Program1
annotateProg prg [] = prg
annotateProg prg (l:ls) = annotateProg (map snd (helper idxPrg l)) ls
  where idxPrg = zip [0..] prg
        
helper :: [(Int, Clause1)] -> (Int, Int) -> [(Int, Clause1)]
helper [] _ = []
helper (c@(cid, c1):cs) l@(cid', tid) 
  | cid == cid' = [(cid, annotateClause c1 tid)] ++ cs
  | otherwise = [c] ++ helper cs l

annotateClause :: Clause1 -> Int -> Clause1
annotateClause c@(Clause (Var _) _) _ = c -- Malformed Cause...
annotateClause c@(Clause (Fun _ []) _) _ = c -- Can't annotate missing transformation function
annotateClause (Clause (Fun idx ts) b) n = Clause (Fun idx (annotateTransFunc n ts)) (annotateBody b n)

annotateTransFunc :: Int -> [Term1] -> [Term1]
annotateTransFunc _ [] = []
annotateTransFunc n ((Fun idx ts):[]) = [Fun idx (annotateNthTerm ts n)] -- Last term should always be the transformation function
annotateTransFunc n (t:ts) = [t] ++ annotateTransFunc n ts

annotateNthTerm :: [Term1] -> Int -> [Term1]
annotateNthTerm [] _ = [] -- Should not reach the end of the list
annotateNthTerm (t@(Fun _ _):ts) _ = (t:ts) -- Should be no functions in this list of terms 
annotateNthTerm ((Var v):ts) 0 = [Var (negate $ abs v)] ++ ts
annotateNthTerm (t:ts) n = [t] ++ annotateNthTerm ts (n-1)

annotateBody :: [Term1] -> Int -> [Term1]
annotateBody [] _ = [] -- Nth term should be in the list
annotateBody (x:xs) 0 = [annotateTerm x] ++ xs
annotateBody (x:xs) n = [x] ++ annotateBody xs (n-1)

annotateTerm :: Term1 -> Term1
annotateTerm (Fun idx ts) = Fun idx (annotateLast ts)
annotateTerm (Var v) = Var (negate ( abs v))

annotateLast :: [Term1] -> [Term1]
annotateLast [] = []
annotateLast ((Var v):[]) = [Var (negate $ abs v)]
annotateLast (t:ts) = [t] ++ annotateLast ts

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
transformHead _ _ _ v@(Var _) = v 
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
addVar _ v@(Var _) = v


