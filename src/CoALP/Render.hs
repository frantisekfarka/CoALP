-- | Tree rendering helpers
--
-- Assumes ImageMagic utility /display/ and Graphviz utility /dot/,
-- both utilies should be in $PATH.
module CoALP.Render (
	  renderProgram
	, ppProgram
	, ppClause
	, ppTerm
  , ppSubst
	, displayProgram
	, displayRewTree
	, displayDerTree
	, displayDerTreeUnsafe
	, displayObsTree
) where

import System.Process

import CoALP.Program (
	  Program
	, Clause(..)
	, Term(..)
	, RewTree1
	, RewTree(..)
	, AndNode(..)
	, OrNode(..)
	, Vr
	, DerTree1
	, DerTree(..)
	, Trans(..)
	, Trans1
	, OTree(..)
	, OTree1
	, OTrans(..)
	, OTrans1
	, DerTree1
	, Trans1,
	)
import CoALP.Parser.PrettyPrint (ppTerm,ppClause,ppSubst, ppProgram, ppSubst)

import CoALP.Guards (gcRewTree,guardingContext)
import CoALP.DerTree (clauseProj)

-- | Display the whole program
displayProgram :: (Show b, Show c) => Program String b c -> IO ()
displayProgram p = do
	saveProgram p "/tmp/test.dot"
	_ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
	return ()

-- | Display rewriting tree up to depth
displayRewTree :: (Show a, Show b, Show c, Show d, Integral d) => Int -> RewTree a b c d -> IO ()
displayRewTree depth rt = do
	writeFile "/tmp/test.dot" (renderRewT "digraph G" depth rt 1)
	_ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
	return ()

-- | Display derivation tree up to depthD, render rewriting trees in nodeѕ up to depthR
displayDerTree :: (Show a, Show b, Show c, Eq a, Eq b, Ord c) => Int -> Int -> DerTree a b c Integer -> IO ()
displayDerTree depD depR dt = -- trace "Display der tree ... " $
	do
		writeFile "/tmp/test.dot" (renderDerT depD depR dt)
		_ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
		return ()

-- | Display der tree ignoring guardednes check
displayDerTreeUnsafe :: Int -> Int -> DerTree1 -> IO ()
displayDerTreeUnsafe depD depR dt = -- trace "Display der tree ... " $
	do
		writeFile "/tmp/test.dot" (renderDerTUns depD depR dt)
		-- _ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
		_ <- spawnCommand "dot -T svg /tmp/test.dot | display"
		return ()

-- | Display observation tree
displayObsTree :: Int -> Int -> OTree1 -> IO ()
displayObsTree depD depR ot = do
	writeFile "/tmp/test.dot" (renderObsT depD depR ot)
	_ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
	return ()

-- | Save program to FilePath
saveProgram :: (Show b, Show c) => Program String b c -> FilePath -> IO ()
saveProgram p f = writeFile f (renderProgram p)

-- | Render Clauses in program
renderProgram :: (Show b, Show c) => Program String b c -> String
renderProgram cl =
	"digraph G {\n" ++
	"\tnode [fontname=\"Monospace\"];\n" ++
	concat (zipWith renderClause [1..] cl) ++
	"}\n"

-- | Render single clause
renderClause :: (Show b, Show c) => Integer ->  Clause String b c -> String
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



-- | Render term in imagemagic dot format
renderTerm :: (Show b, Show c) => Integer -> Term String b c -> String
renderTerm m t0 = (node m t0) ++ (edge m t0)
	where
	--go :: ONode Occ -> Integer -> (Integer, String)
	node :: (Show b, Show c) => Integer -> Term String b c -> String
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
	edge :: (Show b, Show c) => Integer -> Term String b c -> String
	edge _ (Var _) = ""
	edge n (Fun _f t) =
		concat (zipWith (\o _ -> "\t" ++ show n ++ " -> " ++ show o ++ ";\n") [10*n + i  | i <- [1..]] t) ++
		concat (zipWith edge [10*n + i  | i <- [1..]] t)

-- | Render derivation tree
renderRewT :: (Show a, Show b, Show c, Show d, Integral d) => String -> Int -> RewTree a b c d -> Integer -> String
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

-- | Render rewriting tree
renderRewT' :: (Show a, Show b, Show c, Show d, Integral d) => String -> Int -> RewTree a b c d -> Integer -> String
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

-- | Render and node of rewriting tree
renderRewAnd :: (Show a, Show b, Show c, Show d, Integral d) => Integer -> String -> Int -> Integer -> AndNode (Clause a b c) (Term a b c) (Vr d) -> String
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

-- | Render or node of rewriting tree
renderRewOr :: (Show a, Show b, Show c, Show d, Integral d) => Integer -> String -> Int -> Integer -> OrNode (Clause a b c) (Term a b c) (Vr d) -> String
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

-- | box length helper
lh :: String -> String
lh s = show ( fromIntegral (length s) * (0.15 :: Float) )


-- | Render derivation tree
renderDerT :: (Show a, Show b, Show c, Eq a, Eq b, Ord c) => Int -> Int -> DerTree a b c Integer -> String
renderDerT depD depR dt =
	"digraph D {\n" ++
	renderDer depD depR 1 dt ++
	"}\n"

renderDerTUns :: Int -> Int -> DerTree1 -> String
renderDerTUns depD depR dt =
	"digraph D {\n" ++
	renderDerUns depD depR 1 dt ++
	"}\n"

-- | Render observation tree
renderObsT :: Int -> Int -> OTree1 -> String
renderObsT depD depR dt =
	"digraph D {\n" ++
	renderObs depD depR 1 dt ++
	"}\n"

-- | Render derivation tree
renderDer :: (Show a, Show b, Show c, Eq a, Eq b, Ord c) => Int -> Int -> Integer -> DerTree a b c Integer -> String
renderDer 0 _ n _ =
	"\troot" ++ show (n*10) ++ "[shape=box,style=dashed,color=grey,label=\"...\",fixedsize=false];\n" ++
	""
renderDer depD depR n (DT rt trans) = case gcRewTree rt of
	False	-> renderRewT' ("\tsubgraph cluster_" ++ show n) depR rt (10*n)
	True	-> renderRewT ("\tsubgraph cluster_" ++ show n) depR rt (10*n) ++
		concat (zipWith (\x -> renderTrans (10*n) (depD - 1) depR x rt) [10*n + i | i <- [1..]] (take 10 trans))

-- | Render transition
renderTrans :: (Show a, Show b, Show c, Eq a, Eq b, Ord c) => Integer -> Int -> Int -> Integer -> RewTree a b c Integer -> Trans a b c Integer -> String
renderTrans sn depD depR n rt (Trans p _ vr _ gc dt) =
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
renderTransUns sn depD depR n rt (Trans p _ vr _ gc dt) =
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



-- | Render observatin tree
renderObs :: Int -> Int -> Integer -> OTree1 -> String
renderObs 0 _ n _ =
	"\troot" ++ show (n*10) ++ "[shape=box,style=dashed,color=grey,label=\"...\",fixedsize=false];\n" ++
	""
renderObs _depD depR n (UNRT rt ) =
	renderRewT' ("\tsubgraph cluster_" ++ show n) depR rt (10*n)
renderObs depD depR n (ODT rt trans) =
	renderRewT ("\tsubgraph cluster_" ++ show n) depR rt (10*n) ++
	concat (zipWith (\x -> renderOTrans (10*n) (depD - 1) depR x) [10*n + i | i <- [1..]] trans)

-- | Render observation tree transition
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
