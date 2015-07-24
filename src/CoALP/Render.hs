-- | Render some trees
module CoALP.Render (
	  renderProgram
	, displayProgram
	, displayRewTree
	, displayDerTree
	, displayDerTreeUnsafe
	, displayObsTree
) where

import Debug.Trace 
--import Data.Foldable
--import Numeric (showHex)
import System.Process

import CoALP.Program (Program1,Clause1, Clause(..),Term1,Term(..),RewTree1,RewTree(..),
	AndNode(..),OrNode(..),Vr1,
	DerTree1,DerTree(..),Trans(..),Trans1,
	-- GuardingContext,
	OTree(..),OTree1,OTrans(..),OTrans1,DerTree1,Trans1,
	)
import CoALP.Parser.PrettyPrint (ppTerm,ppClause,ppSubst)

import CoALP.Guards (gcRewTree,guardingContext)
import CoALP.DerTree (clauseProj)

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
displayDerTree depD depR dt = -- trace "Display der tree ... " $ 
	do
		writeFile "/tmp/test.dot" (renderDerT depD depR dt)
		_ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
		return ()
	
displayDerTreeUnsafe :: Int -> Int -> DerTree1 -> IO ()
displayDerTreeUnsafe depD depR dt = -- trace "Display der tree ... " $ 
	do
		writeFile "/tmp/test.dot" (renderDerTUns depD depR dt)
		-- _ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
		_ <- spawnCommand "dot -T svg /tmp/test.dot > unsafe.svg"
		return ()
	
displayObsTree :: Int -> Int -> OTree1 -> IO ()
displayObsTree depD depR ot = do
	writeFile "/tmp/test.dot" (renderObsT depD depR ot)
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

renderClause :: Integer ->  Clause1 -> String
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
renderTerm :: Integer -> Term1 -> String
renderTerm m t0 = (node m t0) ++ (edge m t0)
	where
	--go :: ONode Occ -> Integer -> (Integer, String)
	node :: Integer -> Term1 -> String
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
	edge :: Integer -> Term1 -> String
	edge _ (Var _) = ""
	edge n (Fun _f t) = 
		concat (zipWith (\o _ -> "\t" ++ show n ++ " -> " ++ show o ++ ";\n") [10*n + i  | i <- [1..]] t) ++
		concat (zipWith edge [10*n + i  | i <- [1..]] t)

	
renderRewT :: String -> Int -> RewTree1 -> Integer -> String
renderRewT pref _ RTEmpty n = 
	pref ++ " {\n" ++ 
	"\tstyle=dashed;color=grey;\n" ++
	"\tnode [fontname=\"Monospace\"];\n" ++
	"\troot" ++ show n ++ "[shape=box,color=blue,width=2,label=\"_|_\",fixedsize=false];\n" ++
	"}\n"
renderRewT pref depth (RT q s os) n = -- trace "Render Rew ..." $ 
	pref ++ " {\n" ++ 
	"\tstyle=dashed;color=grey;\n" ++
	"\tnode [fontname=\"Monospace\"];\n" ++
	"\t" ++ nid ++ "[shape=box,color=blue,width=" ++ lh lbl ++ ",label=\"" ++ lbl ++ "\",fixedsize=false];\n" ++
	concat (zipWith (renderRewAnd n nid (depth-1)) [10*n + i  | i <- [1..]] os) ++
	"}\n"
	where 
		lbl = ppClause q ++ " | " ++ ppSubst s
		nid = "root" ++ show n


renderRewT' :: String -> Int -> RewTree1 -> Integer -> String
renderRewT' pref _ (RTEmpty) n = pref ++ " {\n" ++ 
	"\tstyle=dashed;color=grey;\n" ++
	"\tnode [fontname=\"Monospace\"];\n" ++
	"\troot" ++ show n ++ "[shape=box,color=blue,width=2,label=\"_|_\",fixedsize=false];\n" ++
	"}\n"
renderRewT' pref depth (RT c s os) n = pref ++ " {\n" ++ 
		"\tstyle=dashed;color=grey;\n" ++
		"\tnode [fontname=\"Monospace\"];\n" ++
		"\t" ++ nid ++ "[shape=box,color=blue,width=" ++ lh lbl ++ ",label=\"" ++ lbl ++ "\",fixedsize=false];\n" ++
		concat (zipWith (renderRewAnd n nid (depth - 1)) [10*n + i  | i <- [1..]] os) ++
		"}\n"
	where
		lbl = "unguarded\\n" ++ ppClause c ++ " | " ++ ppSubst s 
		nid = "root" ++ show n

renderRewAnd :: Integer -> String -> Int -> Integer -> AndNode Clause1 Term1 Vr1 -> String
renderRewAnd _ par 0 n _ = -- trace ("Render and in 0") $
	"\t" ++ show n ++ "[shape=box,color=white,width=.4,label=\"" ++ 
	"..." ++ "\",fixedsize=true];\n" ++
	par ++ " -> " ++ show n ++ ";\n" ++
	""

renderRewAnd sn par depth n (AndNode t ors) = -- trace ("Render and in " ++ show depth) $
	"\t" ++ show n ++ "[shape=box,color=white,width=" ++ lh (ppTerm t) ++ ",label=\"" ++ 
	ppTerm t ++ "\",fixedsize=true];\n" ++
	concat (zipWith (renderRewOr sn (show n) (depth - 1)) [10*n + i | i <- [1..]] ors) ++
	par ++ " -> " ++ show n ++ ";\n" ++
	""

renderRewOr :: Integer -> String -> Int -> Integer -> OrNode Clause1 Term1 Vr1 -> String
renderRewOr _sn par 0 n _ = -- trace ("Render or in 0")
	"\t" ++ show n ++ "[shape=box,color=white,width=.4,label=\"" ++ 
	"..." ++ "\",fixedsize=true];\n" ++
	par ++ " -> " ++ show n ++ ";\n" ++
	""
renderRewOr sn par _depth _n (OrNodeEmpty x) =
	"\t" ++ show x ++ "_" ++ show sn ++ "[shape=box,color=green,width=" ++ lh (show x) ++ ",label=\"" ++ 
	show x ++  "\",fixedsize=true];\n" ++
	par ++ " -> " ++ show x ++ "_" ++ show sn ++ ";\n" ++
	""
renderRewOr sn par depth n (OrNode c ands) = -- trace ("Render or in " ++ show depth)
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

renderDerTUns :: Int -> Int -> DerTree1 -> String
renderDerTUns depD depR dt = 
	"digraph D {\n" ++ 
	renderDerUns depD depR 1 dt ++
	"}\n"

renderObsT :: Int -> Int -> OTree1 -> String
renderObsT depD depR dt = 
	"digraph D {\n" ++ 
	renderObs depD depR 1 dt ++
	"}\n"

renderDer :: Int -> Int -> Integer -> DerTree1 -> String
renderDer 0 _ n _ = 
	"\troot" ++ show (n*10) ++ "[shape=box,style=dashed,color=grey,label=\"...\",fixedsize=false];\n" ++ 
	""
renderDer depD depR n (DT rt trans) = case gcRewTree rt of
	False	-> renderRewT' ("\tsubgraph cluster_" ++ show n) depR rt (10*n)
	True	-> renderRewT ("\tsubgraph cluster_" ++ show n) depR rt (10*n) ++
		concat (zipWith (\x -> renderTrans (10*n) (depD - 1) depR x rt) [10*n + i | i <- [1..]] (take 10 trans))

renderTrans :: Integer -> Int -> Int -> Integer -> RewTree1 -> Trans1 -> String
renderTrans sn depD depR n rt (Trans p _ vr gc dt) =  
	"\t" ++ show n ++ "[shape=diamond,color=green,width=" ++ lh lbl ++ ",label=\"" ++ lbl ++ "\",fixedsize=false];\n" ++ 
	renderDer depD depR (10*n) dt ++
	show vr ++ "_" ++ show sn ++ "-> " ++ show n ++ ";\n" ++
	show n ++ "-> root" ++ show (100*n) ++ ";\n"
	where
		lbl = "gc: {" ++ (f $ guardingContext p rt gc) ++ "}" 
			++ ", cp: {" ++ (f $ clauseProj p gc) ++ "}"
		--lbl = "gc: {" ++ (f $ clauseProj p gc) ++ "}"
		f a = concatMap g a
		g ((ix, t, v)) = "( " ++ show ix ++ ", " ++ ppTerm t ++ ", " ++ show v ++ "),"


renderDerUns :: Int -> Int -> Integer -> DerTree1 -> String
renderDerUns 0 _ n _ = 
	"\troot" ++ show (n*10) ++ "[shape=box,style=dashed,color=grey,label=\"...\",fixedsize=false];\n" ++ 
	""
renderDerUns depD depR n (DT rt trans) = -- case gcRewTree rt of
	--False	-> renderRewT' ("\tsubgraph cluster_" ++ show n) depR rt (10*n) ++
	--	concat (zipWith (\x -> renderTransUns (10*n) (depD - 1) depR x rt) [10*n + i | i <- [1..]] (take 10 trans))
	--True	->
		renderRewT ("\tsubgraph cluster_" ++ show n) depR rt (10*n) ++
		concat (zipWith (\x -> renderTransUns (10*n) (depD - 1) depR x rt) [10*n + i | i <- [1..]] (take depR trans))

renderTransUns :: Integer -> Int -> Int -> Integer -> RewTree1 -> Trans1 -> String
renderTransUns sn depD depR n rt (Trans p _ vr gc dt) =  
	"\t" ++ show n ++ "[shape=diamond,color=green,width=" ++ lh lbl ++ ",label=\"" ++ lbl ++ "\",fixedsize=false];\n" ++ 
	renderDerUns depD depR (10*n) dt ++
	show vr ++ "_" ++ show sn ++ "-> " ++ show n ++ ";\n" ++
	show n ++ "-> root" ++ show (100*n) ++ ";\n"
	where
		lbl = "gc: {" ++ (f $ guardingContext p rt gc) ++ "}" 
			++ ", cp: {" ++ (f $ clauseProj p gc) ++ "}"
		--lbl = "gc: {" ++ (f $ clauseProj p gc) ++ "}"
		f a = concatMap g a
		g ((ix, t, v)) = "( " ++ show ix ++ ", " ++ ppTerm t ++ ", " ++ show v ++ "),"



renderObs :: Int -> Int -> Integer -> OTree1 -> String
renderObs 0 _ n _ = 
	"\troot" ++ show (n*10) ++ "[shape=box,style=dashed,color=grey,label=\"...\",fixedsize=false];\n" ++ 
	""
renderObs _depD depR n (UNRT rt ) = 
	renderRewT' ("\tsubgraph cluster_" ++ show n) depR rt (10*n)
renderObs depD depR n (ODT rt trans) = 
	renderRewT ("\tsubgraph cluster_" ++ show n) depR rt (10*n) ++
	concat (zipWith (\x -> renderOTrans (10*n) (depD - 1) depR x) [10*n + i | i <- [1..]] trans) 

renderOTrans :: Integer -> Int -> Int -> Integer -> OTrans1 -> String
renderOTrans sn _depD _depR n (GTrans vr _ gc) = 
	"\t" ++ show n ++ "[shape=diamond,color=green,width=" ++ lh lbl ++ ",label=\"" ++ lbl ++ "\",fixedsize=false];\n" ++ 
	show vr ++ "_" ++ show sn ++ "-> " ++ show n ++ ";\n"
	where
		lbl = "Guarded trans\\ngc: {" ++ (f gc) ++ "}"
		f a = concatMap g a
		g (ix, t, v) = "( " ++ show ix ++ ", " ++ ppTerm t ++ ", " ++ show v ++ "),"

renderOTrans sn depD depR n (OTrans p rt vr gc dt) =
	"\t" ++ show n ++ "[shape=diamond,color=green,width=" ++ lh lbl ++ ",label=\"" ++ lbl ++ "\",fixedsize=false];\n" ++ 
	renderObs depD depR (10*n) dt ++
	show vr ++ "_" ++ show sn ++ "-> " ++ show n ++ ";\n" ++
	show n ++ "-> root" ++ show (100*n) ++ ";\n"
	where
		lbl = "gc: {" ++ (f $ guardingContext p rt gc) ++ "}"
		f a = concatMap g a
		g ((ix, t, v)) = "( " ++ show ix ++ ", " ++ ppTerm t ++ ", " ++ show v ++ "),"


