-- | Module that transforms logic programs automatically
module CoALP.Transform (
         transformProg
       , transformProgA
       , transformClause
       , addTerm
       , annotateProg
       , annotateProgA
       , annotateClause
       , annotateNth
       , annotateTerm1
       , annotateTermA
       , toProgramA
       , toClauseA
) where

import CoALP.Program (Program1,ProgramA,Clause1,ClauseA,Clause(..),Term1,TermA,Term(..), AnnoVar(..))
--import CoALP.Guards

-- TODO Could annotate multiple terms in a single clause at once

annotateProg :: Program1 -> [(Int, Int)] -> Program1
annotateProg prg [] = prg
annotateProg prg (l:ls) = annotateProg (map snd (helper annotateTerm1 idxPrg l)) ls
  where idxPrg = zip [0..] prg

annotateProgA :: ProgramA -> [(Int, Int)] -> ProgramA
annotateProgA prgA [] = prgA
annotateProgA prgA (l:ls) = annotateProgA (map snd (helper annotateTermA idxPrg l)) ls
  where idxPrg = zip [0..] prgA
        
helper :: (Term a b c -> Term a b c) -> [(Int, Clause a b c)] -> (Int, Int) -> [(Int, Clause a b c)]
helper _ [] _ = []
helper annoF (c@(cid, c1):cs) l@(cid', tid) 
  | cid == cid' = [(cid, annotateClause annoF c1 tid)] ++ cs
  | otherwise = [c] ++ helper annoF cs l

annotateClause :: (Term a b c -> Term a b c) -> Clause a b c -> Int -> Clause a b c
annotateClause _ c@(Clause (Var _) _) _ = c -- Malformed Cause...
annotateClause _ c@(Clause (Fun _ []) _) _ = c -- Can't annotate missing transformation function
annotateClause annoF  (Clause (Fun idx ts) b) n = Clause (Fun idx (init ts ++ [nf])) (annotateNth annoF b n)
  where Fun i xs = last ts  -- Last function in the head should be the transformation fuction
        nf       = Fun i (annotateNth annoF xs n)

annotateNth :: (Term a b c -> Term a b c) -> [Term a b c] -> Int -> [Term a b c]
annotateNth annoF xs i 
  | i < length xs && i >= 0 = frnt ++ [annoF y] ++ ys -- Annotate the nth term
  | otherwise               = xs
       where (frnt, (y:ys)) = splitAt i xs


annotateTerm1 :: Term1 -> Term1
annotateTerm1 f@(Fun _ []) = f
-- If it's a function it is from the body so last term is the transformation variable to annotate
annotateTerm1 (Fun idx ts) = Fun idx (init ts ++ [annotateTerm1 (last ts)]) 
annotateTerm1 (Var v) = Var (negate ( abs v))

annotateTermA :: TermA -> TermA
annotateTermA f@(Fun _ [])  = f
-- If it's a function it is from the body so last term is the transformation variable to annotate
annotateTermA (Fun idx ts)  = Fun idx (init ts ++ [annotateTermA (last ts)]) 
annotateTermA (Var (Ind v)) = Var (CoInd v)
annotateTermA vr@(Var _)    = vr

--annotate :: a -> b
--annotate v = CoIn v

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

transformProgA :: (ProgramA, Integer) -> (ProgramA, Integer)
transformProgA prgWithCount = transformProgAAux prgWithCount transFuncs
  where transFuncs = map (\x -> Fun ("transform-func-" ++ show x) []) [(1::Int)..]

transformProgAAux :: (ProgramA, Integer) -> [TermA] -> (ProgramA, Integer)
transformProgAAux ([], i) _ = ([],i)
transformProgAAux (_, i) [] = ([],i)
transformProgAAux ((x@(Clause _ b):xs), count) (tf:tfs) = ([transformed] ++ rest, lastNewVar)
  where (rest, lastNewVar) = transformProgAAux (xs, lastVar) tfs
        numTerms = toInteger . length $ b
        lastVar = count + numTerms
        transformed = transformClause (foldl addTerm tf (map (Var . Ind) [count..lastVar-1])) x

-- Transforms a clause
transformClause :: Term a b c -> Clause a b c -> Clause a b c
transformClause nf@(Fun _ ts) (Clause h b) = Clause (addTerm h nf) (zipWith addTerm b ts)
transformClause (Var _) c = c 

-- Adds a Term to the end of a function term
-- term - term to add
addTerm :: Term a b c -> Term a b c -> Term a b c
addTerm (Fun ids ts) var = Fun ids (ts ++ [var])
addTerm v@(Var _) _ = v

toClauseA :: Clause1 -> ClauseA
toClauseA c = fmap (Ind) c

toProgramA :: Program1 -> ProgramA
toProgramA p = map (fmap Ind) p

