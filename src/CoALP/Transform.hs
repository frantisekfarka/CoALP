-- | Module that transforms logic programs automatically
module CoALP.Transform (
         transformProg
       , transformClause
       , addTerm
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
transformProg prgWithCount =  transformProgAux prgWithCount transFuncs
  where transFuncs = map (\x -> Fun ("transform-func-" ++ show x) []) [(1::Int)..]

transformProgAux :: (Program1, Integer) -> [Term1] -> Program1
transformProgAux ([], _) _ = []
transformProgAux _ [] = []
transformProgAux ((x@(Clause _ b):xs), count) (tf:tfs) = [transformed] ++ transformProgAux (xs, lastVar) tfs
  where numTerms = toInteger . length $ b
        lastVar = count + numTerms
        transformed = transformClause (foldl addTerm tf (map Var [count..lastVar-1])) x

-- Transforms a clause
transformClause :: Term a b c -> Clause a b c -> Clause a b c
transformClause nf@(Fun _ ts) (Clause h b) = Clause (addTerm h nf) (zipWith addTerm b ts)
transformClause (Var _) c = c 

-- Adds a Term to the end of a function term
-- term - term to add
addTerm :: Term a b c -> Term a b c -> Term a b c
addTerm (Fun ids ts) var = Fun ids (ts ++ [var])
addTerm v@(Var _) _ = v


