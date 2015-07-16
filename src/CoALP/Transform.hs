-- | Module that transforms logic programs automatically
module CoALP.Transform (
         transformProg
       , transformClause
       , addTerm
       , annotateProg
       , annotateClause
       , annotateNth
       , annotateTerm
) where

import CoALP.Program (Program1,Clause1, Clause(..),Term1,Term(..))
--import CoALP.Guards

-- TODO Could annotate multiple terms in a single clause at once

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
annotateClause (Clause (Fun idx ts) b) n = Clause (Fun idx (init ts ++ [nf])) (annotateNth b n)
  where Fun i xs = last ts  -- Last function in the head should be the transformation fuction
        nf       = Fun i (annotateNth xs n)

annotateNth :: [Term1] -> Int -> [Term1]
annotateNth xs i 
  | i < length xs && i >= 0 = frnt ++ [annotateTerm y] ++ ys -- Annotate the nth term
  | otherwise               = xs
       where (frnt, (y:ys)) = splitAt i xs

annotateTerm :: Term1 -> Term1
annotateTerm f@(Fun _ []) = f
-- If it's a function it is from the body so last term is the transformation variable to annotate
annotateTerm (Fun idx ts) = Fun idx (init ts ++ [annotateTerm (last ts)]) 
annotateTerm (Var v) = Var (negate ( abs v))

-- Transform a program
-- prgWithCount - pair with the program to transform and the next fresh variable
transformProg :: (Program1, Integer) -> (Program1, Integer)
transformProg prgWithCount =  transformProgAux prgWithCount transFuncs
  where transFuncs = map (\x -> Fun ("transform-func-" ++ show x) []) [(1::Int)..]

transformProgAux :: (Program1, Integer) -> [Term1] -> (Program1, Integer)
transformProgAux ([], i) _ = ([],i)
transformProgAux (_, i) [] = ([],i)
transformProgAux ((x@(Clause _ b):xs), count) (tf:tfs) = ([transformed] ++ rest, lastNewVar)
  where (rest, lastNewVar) = transformProgAux (xs, lastVar) tfs
        numTerms = toInteger . length $ b
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


