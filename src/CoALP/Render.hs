-- | Render some trees
module CoALP.Render (
	  renderProgram
	, displayProgram
	, displayRewTree
	, displayDerTree
) where

--import Data.Foldable
--import Numeric (showHex)
import System.Process

import CoALP.DerTree (clauseProj)
import CoALP.Program (Program1,Clause1, Clause(..),Term1,Term(..),RewTree1,RewTree(..),
	AndNode(..),OrNode(..),Vr1,
	DerTree1,DerTree(..),Trans(..),Trans1
	)
import CoALP.Parser.PrettyPrint (ppTerm,ppClause,ppQuery,ppSubst)



-- | TODO refactor! -- use tree language!
displayProgram :: Program1 -> IO ()
displayProgram p = do
	saveProgram p "/tmp/test.dot"
	_ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
	return ()

displayRewTree :: Int -> RewTree1 -> IO ()
displayRewTree depth rt = do
	writeFile "/tmp/test.dot" (renderRewT "digraph G" depth rt 1)
	_ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
	return ()
	
displayDerTree :: Int -> Int -> DerTree1 -> IO ()
displayDerTree depD depR dt = do
	writeFile "/tmp/test.dot" (renderDerT depD depR dt)
	_ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
	return ()
	
saveProgram :: Program1 -> FilePath -> IO ()
saveProgram p f = writeFile f (renderProgram p)

renderProgram :: Program1 -> String
renderProgram cl = 
	"digraph G {\n" ++ 
	"\tnode [fontname=\"Monospace\"];\n" ++
	concat (zipWith renderClause [1..] cl) ++
	"}\n"

renderClause :: Int ->  Clause1 -> String
renderClause n (c@(Clause h b)) =
	"subgraph cluster" ++ show n ++ "{\n" ++
	"\tcolor=grey;label=\"" ++ helper  ++ "\";\n" ++
	"\t" ++ show n ++ "[color=red,shape=record,fixedsize=false,label=\"<f1> _ | :- | <f2> _ \"];\n" ++
	renderTerm (10*n) h ++
	"\t" ++ show n  ++ ":f1 -> " ++ show (10*n) ++ ";\n" ++
	concat (zipWith renderTerm [20*n + i  | i <- [1..]] b) ++
	concat (zipWith (\m _ -> "\t" ++ show n ++ ":f2 -> " ++ show m ++ ";\n") [20*n + i  | i <- [1..]] b) ++ "\n" ++
	"}\n"

	where
	helper = if null b 
		then ppClause c
		else ppTerm h ++ " :- ..."



-- | render in imagemagic dot format
-- TODO refactor
-- render :: ONode Occ -> String
renderTerm :: Int -> Term1 -> String
renderTerm m t0 = (node m t0) ++ (edge m t0)
	where
	--go :: ONode Occ -> Int -> (Int, String)
	node :: Int -> Term1 -> String
	node n (Var i) =
		"\t" ++ show n
		++ "[color=white,label=\"V_" ++ show i ++ "\"];\n"
--	node n (Const i) =
--		"\t" ++ show n
--		++ "[color=white,label=\"" ++ show i ++ "\"];\n"
	node n (Fun f t) =
		"\t" ++ show n ++
		"[shape=box,color=white,width=.2,label=\"" ++ f ++ "\",fixedsize=false];\n" ++
		concat (zipWith node [10*n + i  | i <- [1..]] t)
	edge :: Int -> Term1 -> String
	edge _ (Var _) = ""
	edge n (Fun _f t) = 
		concat (zipWith (\o _ -> "\t" ++ show n ++ " -> " ++ show o ++ ";\n") [10*n + i  | i <- [1..]] t) ++
		concat (zipWith edge [10*n + i  | i <- [1..]] t)

	
renderRewT :: String -> Int -> RewTree1 -> Int -> String
renderRewT pref _ RTEmpty n = 
	pref ++ " {\n" ++ 
	"\tstyle=dashed;color=grey;\n" ++
	"\tnode [fontname=\"Monospace\"];\n" ++
	"\troot" ++ show n ++ "[shape=box,color=blue,width=2,label=\"_|_\",fixedsize=false];\n" ++
	"}\n"
renderRewT pref depth (RT q s os) n = 
	pref ++ " {\n" ++ 
	"\tstyle=dashed;color=grey;\n" ++
	"\tnode [fontname=\"Monospace\"];\n" ++
	"\t" ++ nid ++ "[shape=box,color=blue,width=" ++ lh lbl ++ ",label=\"" ++ lbl ++ "\",fixedsize=false];\n" ++
	concat (zipWith (renderRewAnd n nid (depth-1)) [10*n + i  | i <- [1..]] os) ++
	"}\n"
	where 
		lbl = ppClause q ++ " | " ++ ppSubst s
		nid = "root" ++ show n


renderRewAnd :: Int -> String -> Int -> Int -> AndNode Clause1 Term1 Vr1 -> String
renderRewAnd _ par 0 n _ = 
	"\t" ++ show n ++ "[shape=box,color=white,width=.4,label=\"" ++ 
	"..." ++ "\",fixedsize=true];\n" ++
	par ++ " -> " ++ show n ++ ";\n" ++
	""

renderRewAnd sn par depth n (AndNode t ors) = 
	"\t" ++ show n ++ "[shape=box,color=white,width=" ++ lh (ppTerm t) ++ ",label=\"" ++ 
	ppTerm t ++ "\",fixedsize=true];\n" ++
	concat (zipWith (renderRewOr sn (show n) (depth - 1)) [10*n + i | i <- [1..]] ors) ++
	par ++ " -> " ++ show n ++ ";\n" ++
	""

renderRewOr :: Int -> String -> Int -> Int -> OrNode Clause1 Term1 Vr1 -> String
renderRewOr _sn par 0 n _ = 
	"\t" ++ show n ++ "[shape=box,color=white,width=.4,label=\"" ++ 
	"..." ++ "\",fixedsize=true];\n" ++
	par ++ " -> " ++ show n ++ ";\n" ++
	""
renderRewOr sn par _depth _n (OrNodeEmpty x) =
	"\t" ++ show x ++ "_" ++ show sn ++ "[shape=box,color=green,width=" ++ lh (show x) ++ ",label=\"" ++ 
	show x ++  "\",fixedsize=true];\n" ++
	par ++ " -> " ++ show x ++ "_" ++ show sn ++ ";\n" ++
	""
renderRewOr sn par depth n (OrNode c ands) = 
	"\t" ++ nid ++ "[shape=box,color=white,width=" ++ lh (ppClause c) ++ ",label=\"" ++ 
	ppClause c ++ "\",fixedsize=true];\n" ++
	concat (zipWith (renderRewAnd sn nid (depth - 1)) [10*n + i  | i <- [1..]] ands) ++
	par ++ " -> " ++ nid ++ ";\n" ++
	""
	where
		nid = show n


lh :: String -> String
lh s = show ( fromIntegral (length s) * (0.15 :: Float) )


	
renderDerT :: Int -> Int -> DerTree1 -> String
renderDerT depD depR dt = 
	"digraph D {\n" ++ 
	renderDer depD depR 1 dt ++
	"}\n"

renderDer :: Int -> Int -> Int -> DerTree1 -> String
renderDer 0 _ n _ = 
	"\troot" ++ show (n*10) ++ "[shape=box,style=dashed,color=grey,label=\"...\",fixedsize=false];\n" ++ 
	""
renderDer depD depR n (DT rew trans) = 
	renderRewT ("\tsubgraph cluster_" ++ show n) depR rew (10*n) ++
	concat (zipWith (renderTrans (10*n) (depD - 1) depR) [10*n + i | i <- [1..]] trans) 

renderTrans :: Int -> Int -> Int -> Int -> Trans1 -> String
renderTrans sn depD depR n (Trans p vr cp dt) =
	"\t" ++ show n ++ "[shape=diamond,color=green,width=" ++ lh lbl ++ ",label=\"" ++ lbl ++ "\",fixedsize=false];\n" ++ 
	renderDer depD depR (10*n) dt ++
	show vr ++ "_" ++ show sn ++ "-> " ++ show n ++ ";\n" ++
	show n ++ "-> root" ++ show (100*n) ++ ";\n"
	where
		lbl = "gc: {" ++ (f $ clauseProj p cp) ++ "}"
		f a = concatMap g a
		g ((ix, t, v)) = "( " ++ show ix ++ ", " ++ ppTerm t ++ ", " ++ show v ++ "),"




