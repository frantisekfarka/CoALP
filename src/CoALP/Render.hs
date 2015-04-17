-- | Render some trees
module CoALP.Render (
	  renderProgram
	, displayProgram
	, displayRewTree
) where

--import Data.Foldable
import System.Process

import CoALP.Program (Program1,Clause1, Clause(..),Term1,Term(..),RewTree1,RewTree(..),
	AndNode(..),OrNode(..),Query(..),Query1,Vr(..),Vr1
	)
import CoALP.Parser.PrettyPrint (ppTerm,ppClause,ppQuery)


-- | TODO refactor! -- use tree language!
displayProgram :: Program1 -> IO ()
displayProgram p = do
	saveProgram p "/tmp/test.dot"
	_ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
	return ()

displayRewTree :: Int -> RewTree1 -> IO ()
displayRewTree depth rt = do
	putStrLn (renderRewT depth rt)
	writeFile "/tmp/test.dot" (renderRewT depth rt)
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
	"\tcolor=grey,label=\"" ++ helper  ++ "\";\n" ++
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



--renderTerm h

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
		++ "[color=white,label=\"_v" ++ show i ++ "\"];\n"
--	node n (Const i) =
--		"\t" ++ show n
--		++ "[color=white,label=\"" ++ show i ++ "\"];\n"
	node n (Fun f t) =
		"\t" ++ show n ++
		"[shape=box,color=white,width=.2,label=\"" ++ f ++ "\",fixedsize=false];\n" ++
		concat (zipWith node [10*n + i  | i <- [1..]] t)
	edge :: Int -> Term1 -> String
	edge n (Var _) = ""
--	edge n (Const _) = ""
	edge n (Fun f t) = 
		concat (zipWith (\o _ -> "\t" ++ show n ++ " -> " ++ show o ++ ";\n") [10*n + i  | i <- [1..]] t) ++
		concat (zipWith edge [10*n + i  | i <- [1..]] t)

	
renderRewT :: Int -> RewTree1 -> String
renderRewT 0 _ = ""
renderRewT depth rt@(RT q s os) = 
	"digraph G {\n" ++ 
	"\tnode [fontname=\"Monospace\"];\n" ++
	"\troot[shape=box,color=blue,width=" ++ lh (ppQuery q) ++ ",label=\"" ++ ppQuery q ++ "\",fixedsize=false];\n" ++
	concat (zipWith (renderRewAnd (depth-1)) [i  | i <- [1..]] os) ++
	concatMap (\o -> "\troot -> " ++ show o ++ ";\n") [i  | i <- [1..(length os)]] ++
	"}\n"


renderRewAnd :: Int -> Int -> AndNode Clause1 Term1 Vr1 -> String
renderRewAnd 0 _ _ = ""
renderRewAnd depth n (AndNode t ors) = 
	"\t" ++ show n ++ "[shape=box,color=white,width=" ++ lh (ppTerm t) ++ ",label=\"" ++ 
	ppTerm t ++ "\",fixedsize=true];\n" ++
	concat (zipWith (renderRewOr (depth - 1)) [10*n + i | i <- [1..]] ors) ++
	concat (zipWith
		(\o p -> "\t" ++ show n ++ " -> " ++ show o ++ ";\n")
		[10*n + i  | i <- [1..]] 
		ors
	) ++ ""


renderRewOr :: Int -> Int -> OrNode Clause1 Term1 Vr1 -> String
renderRewOr 0 n _ = 
	"\t" ++ show n ++ "[shape=box,color=white,width=.4,label=\"" ++ 
	"_|_" ++ "\",fixedsize=true];\n" ++
	""
renderRewOr depth n (OrNodeEmpty x) =
	"\t" ++ show n ++ "[shape=box,color=white,width=.4,label=\"" ++ 
	show x ++ "\",fixedsize=true];\n" ++
	""
renderRewOr depth n (OrNode c@(Clause h b) ands) = 
	"\t" ++ show n ++ "[shape=box,color=white,width=" ++ lh (ppClause c) ++ ",label=\"" ++ 
	ppClause c ++ "\",fixedsize=true];\n" ++
	concat (zipWith (renderRewAnd (depth - 1)) [10*n + i  | i <- [1..]] ands) ++
	concatMap (\o -> "\t" ++ show n ++ " -> " ++ show o ++ ";\n") [n*10 + i  | i <- [1..(length ands)]] ++
	""


lh :: String -> String
lh s = show ( fromIntegral (length s) * (0.15 :: Float) )
