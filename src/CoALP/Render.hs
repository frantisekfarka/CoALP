{-# LANGUAGE OverloadedStrings #-}
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

import Data.GraphViz
import Data.GraphViz.Attributes.Complete(Attribute(..), StyleName(..), StyleItem(..))
import Data.Text.Lazy(pack, Text)
import Data.Hashable
--import CoALP.FreshVar (getFresh,evalFresh,Freshable(..),initFresh)

import CoALP.Program (Program, Program1,Clause1, Clause(..),Term1,Term(..),RewTree1,RewTree(..),
	AndNode(..),OrNode(..),Vr1,Vr(..),
	DerTree1,DerTree(..),Trans(..),Trans1,
	-- GuardingContext,
	OTree(..),OTree1,OTrans(..),OTrans1,DerTree1,Trans1,
	)
import CoALP.Parser.PrettyPrint (ppTerm,ppClause,ppSubst, ppProgram, ppSubst)

import CoALP.Guards (gcRewTree,guardingContext)
import CoALP.DerTree (clauseProj)

-- | Display the whole program
displayProgram :: (Show b, Show c, Ord b, Ord c) => Program String b c -> IO ()
displayProgram p = do
	saveProgram'' p "/tmp/test.dot"
	_ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
	return ()

-- | Display rewriting tree up to depth
displayRewTree' :: (Show a, Show b, Show c, Show d, Integral d) => Int -> RewTree a b c d -> IO ()
displayRewTree' depth rt = do
	writeFile "/tmp/test.dot" (renderRewT "digraph G" depth rt 1)
	_ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
	return ()
	
-- | Display derivation tree up to depthD, render rewriting trees in nodeѕ up to depthR
displayDerTree' :: (Show a, Show b, Show c, Eq a, Eq b, Ord c) => Int -> Int -> DerTree a b c Integer -> IO ()
displayDerTree' depD depR dt = -- trace "Display der tree ... " $ 
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

-- graphElemsToDot :: (Ord cl, Ord n) => GraphvizParams n nl el cl l -> [(n, nl)] -> [(n, n, el)] -> DotGraph n

type ClauseEdge a b c = (Term a b c, Term a b c, String)
type ClauseNode a b c= (Term a b c, Text)


saveProgram' :: (Show a, Show b, Show c, Ord a, Ord b, Ord c) => Program a b c -> FilePath -> IO ()
saveProgram' p f = do --map getClauseDetails p
             _ <- runGraphviz (renderClause' (p !! 3)) Canon f 
             return ()


renderClause' :: (Show a, Show b, Show c, Ord a, Ord b, Ord c) => Clause a b c -> DotGraph (Term a b c)
renderClause' (Clause h b) = graphElemsToDot nonClusteredParams nodes edges
       where nodes = (h, ppTerm h):(map (\t -> (t, ppTerm t)) b)
             edges = map (\t -> (h, t, "" :: String)) b

saveProgram'' :: (Show a, Show b, Show c, Ord a, Ord b, Ord c) => Program a b c -> FilePath -> IO ()
saveProgram'' p f = do
             _ <- runGraphviz graph Canon f 
             return ()
       where details = map getClauseDetails idxp 
             idxp = zip [1..] p
             nodes = concat $ map fst details
             edges = concat $ map snd details
             graph = graphElemsToDot params nodes edges
             params = defaultParams { clusterBy = clustBy, clusterID = Str }
             clustBy (n, l) = C l (N (n, ppTerm n))
             --clFmt m = [GraphAttrs [toLabel $ "Cluster " ++ show m]] 

getClauseDetails :: (Show a, Show b, Show c) => (Int,Clause a b c) -> ([ClauseNode a b c],[ClauseEdge a b c])
getClauseDetails (n,(Clause h b)) = (nodes, edges)
       where nodes = (h,(pack $ show n)):(map (\t -> (t, (pack $ show n))) b)
             edges = map (\t -> (h, t, "")) b




-- params = blankParams { globalAttributes = []
--                     , clusterBy        = clustBy
--                     , clusterID        = Str
--                     , fmtCluster       = clFmt
--                     , fmtNode          = const []
--                     , fmtEdge          = const []
 --                    }
--renderTerm' :: (Show a, Show b, Show c, Ord a, Ord b, Ord c) => Term a b c -> DotGraph String
--renderTerm' 

--clauseCluster 
--renderClause'' :: (Show a, Show b, Show c, Ord a, Ord b, Ord c) => Clause a b c -> DotGraph String
--renderClause'' (Clause h b) =  graphElemsToDot nonClusteredParams nodes edges
--       where edges = [] --map (clauseEdges h) b
--             nodes = label (h:b)



--clauseEdges :: (Show a, Show b, Show c) => Term a b c -> Term a b c -> (String, String, String)
--clauseEdges t1 t2 = (ppTerm t1, ppTerm t2, "") 

--label :: (Show a, Show b, Show c) => [Term a b c] -> [(String, String)]
--label ts = map (\t -> (ppTerm t, "")) ts         

type RewNode = ([Integer], StyledText)
type RewEdge = ([Integer], [Integer], String)
type RewNodeClust = ([Integer], Cluster StyledText)

data StyledText = BlueBox Text | GreenBox Text | WhiteBox Text | GreenDiamond Text | EmptyText
data Cluster a  = Cluster [Integer] a

displayRewTree :: (Show a, Show b, Show c, Show d, Integral d) => Int -> RewTree a b c d -> IO ()
displayRewTree depth rt = do
	saveRewT depth rt "/tmp/test.dot"
	_ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
	return ()

saveRewT :: (Show a, Show b, Show c, Show d, Integral d) => Int -> RewTree a b c d -> FilePath -> IO ()
saveRewT depth rt f = do
              _ <- runGraphviz graph Canon f
              return ()
        where (nodes, edges) = getRewTDetails depth [1] rt 1
              graph =  graphElemsToDot params nodes edges
              params = nonClusteredParams { globalAttributes = ga, fmtNode = nStyle }
              ga = [ GraphAttrs [FontName "Monospace"] ] -- FontName doesn't seem to be working correctly.. 

nStyle :: (t, StyledText) -> [Attribute]
nStyle (_, BlueBox l)  = [textLabel l, shape BoxShape, color Blue]
nStyle (_, GreenBox l) = [textLabel l, shape BoxShape, color Green]
nStyle (_, WhiteBox l) = [textLabel l, shape BoxShape, color White]
nStyle (_, GreenDiamond l) = [textLabel l, shape MDiamond, color Green]
nStyle (_, EmptyText) = [textLabel ""]

getRewTDetails :: (Show a, Show b, Show c, Show d, Integral d) => Int -> [Integer] -> RewTree a b c d -> Integer -> ([RewNode], [RewEdge])
getRewTDetails _ par RTEmpty n = ([(par ++ [n], BlueBox $ pack "_|_")],[])
getRewTDetails depth par (RT q s os) n = ([(par ++ [n], BlueBox $ pack $ ppClause q ++ " | " ++ ppSubst s)] ++ nodes, edges)
        where nodes = concat $ map fst children
              edges = concat $ map snd children
              children = zipWith (getRewAndDetails par (par ++ [n]) (depth-1)) [1..] os

getRewAndDetails :: (Show a, Show b, Show c, Show d, Integral d) => [Integer] -> [Integer] -> Int -> Integer -> AndNode (Clause a b c) (Term a b c) (Vr d) -> ([RewNode], [RewEdge])
getRewAndDetails _ par 0 n _ = ([(par ++ [n], WhiteBox $ pack "...")],[(par, par ++ [n], "")])
getRewAndDetails sn par depth n (AndNode t ors) = ([(par ++ [n], WhiteBox $ pack $ ppTerm t)] ++ nodes, [(par, par ++ [n], "")] ++ edges)
        where nodes = concat $ map fst children
              edges = concat $ map snd children
              children = zipWith (getRewOrDetails sn (par ++ [n]) (depth - 1)) [1..] ors


getRewOrDetails :: (Show a, Show b, Show c, Show d, Integral d) => [Integer] -> [Integer] -> Int -> Integer -> OrNode (Clause a b c) (Term a b c) (Vr d) -> ([RewNode], [RewEdge])
getRewOrDetails _sn par 0 n _ = ([(par ++ [n], WhiteBox $ pack "...")], [(par, par ++ [n], "")])
getRewOrDetails sn par _depth _n (OrNodeEmpty x@(Vr v)) = ([(par ++ [(toInteger v)], GreenBox $ pack $ show x )], [(par, par ++ [(toInteger v)], "")])
getRewOrDetails sn par depth n (OrNode c ands) = ([(par ++ [n], WhiteBox $ pack $ ppClause c)] ++ nodes, [(par, par ++ [n], "")] ++ edges)
        where nodes = concat $ map fst children
              edges = concat $ map snd children
              children = zipWith (getRewAndDetails sn (par ++ [n]) (depth -1)) [1..] ands

getRewTTrunc :: (Show a, Show b, Show c, Show d, Integral d) => Int -> [Integer] -> RewTree a b c d -> Integer -> [([Integer], [Integer])]
getRewTTrunc _ par RTEmpty n = []
getRewTTrunc depth par (RT q s os) n = concat $ zipWith (getRewAndTrunc par (par ++ [n]) (depth-1)) [1..] os

getRewAndTrunc ::  (Show a, Show b, Show c, Show d, Integral d) => [Integer] -> [Integer] -> Int -> Integer -> AndNode (Clause a b c) (Term a b c) (Vr d) -> [([Integer], [Integer])]
getRewAndTrunc sn par 0 n (AndNode t ors) = [(par ++ [n], varsTrunc)]
        where varsTrunc = concat $ map getRewOrVars ors
getRewAndTrunc sn par depth n (AndNode t ors) = concat $ zipWith (getRewOrTrunc sn (par ++ [n]) (depth - 1)) [1..] ors

getRewOrTrunc :: (Show a, Show b, Show c, Show d, Integral d) => [Integer] -> [Integer] -> Int -> Integer -> OrNode (Clause a b c) (Term a b c) (Vr d) -> [([Integer], [Integer])]
getRewOrTrunc _sn par 0 n ors = [(par ++ [n], varsTrunc)]
        where varsTrunc = getRewOrVars ors
getRewOrTrunc sn par _depth _n (OrNodeEmpty x@(Vr v)) = []
getRewOrTrunc sn par depth n (OrNode c ands) = concat $ zipWith (getRewAndTrunc sn (par ++ [n]) (depth -1)) [1..] ands
        
getRewAndVars :: (Integral d) => AndNode (Clause a b c) (Term a b c) (Vr d) -> [Integer]
getRewAndVars (AndNode t ors) = concat $ map getRewOrVars ors

getRewOrVars :: (Integral d) => OrNode (Clause a b c) (Term a b c) (Vr d) -> [Integer]
getRewOrVars (OrNodeEmpty x@(Vr v)) = [toInteger v]
getRewOrVars (OrNode c ands) = concat $ map getRewAndVars ands

-- | Display derivation tree up to depthD, render rewriting trees in nodeѕ up to depthR
displayDerTree :: (Show a, Show b, Show c, Eq a, Eq b, Ord c) => Int -> Int -> DerTree a b c Integer -> IO ()
displayDerTree depD depR dt = -- trace "Display der tree ... " $ 
	do
                saveDerT depD depR dt "/tmp/test.dot"
		_ <- spawnCommand "dot -Tpng /tmp/test.dot |  display"
		return ()

saveDerT :: (Show a, Show b, Show c, Eq a, Eq b, Ord c) => Int -> Int -> DerTree a b c Integer -> FilePath -> IO ()
saveDerT depD depR dt f = do
              _ <- runGraphviz graph Canon f
              return ()
        where (nodes, edges) = getDerDetails depD depR [1] 1 dt
              graph =  graphElemsToDot params nodes edges
              params = defaultParams { globalAttributes = ga
                                     , clusterBy = clustByRewT
                                     , isDotCluster = isTrueCluster
                                     , clusterID = Str
                                     , fmtCluster = cStyle
                                     , fmtNode = nStyle }
              ga = [ GraphAttrs [FontName "Monospace"] ] -- FontName doesn't seem to be working correctly.. 
              cStyle _ = [ GraphAttrs [style (SItem Dashed []), color Gray] ]

clustByRewT (n, Cluster i l) = C (pack $ show $ hash i) (N (n, l))

isTrueCluster cl
  | cl == (pack $ show $ hash ([0] :: [Int]) ) = False
  | otherwise  = True

makeCluster :: ([RewNode], [RewEdge]) -> [Integer] -> ([RewNodeClust], [RewEdge])
makeCluster (ns, es) c = (clustered,es)
        where clustered = map (\(n, l) -> (n, Cluster c l)) ns

getDerDetails :: (Show a, Show b, Show c, Eq a, Eq b, Eq c, Ord c) => Int -> Int -> [Integer] -> Integer -> DerTree a b c Integer -> ([RewNodeClust], [RewEdge])
getDerDetails depD depR par n (DT rt trans) = case gcRewTree rt of
        False -> (clNodes, rtEdges)-- Unguarded
        True  -> (clNodes ++ tNodes, rtEdges ++ tEdges)
        where (clNodes, _) = makeCluster ns par
              ns@(rtNodes, rtEdges) = getRewTDetails depR par rt n
              (tNodes, tEdges) = (concat $ map fst transDetails, concat $ map snd transDetails )
              transDetails = (zipWith (\x -> getTransDetails par (depD - 1) depR x rt rtNodes truncation) [1..] trans)
              truncation = getRewTTrunc depR par rt n -- [([Integer], [Integer])]

isTruncParent :: Integer -> ([Integer], [Integer]) -> Bool
isTruncParent n (l, []) = False
isTruncParent n (l,(x:xs)) 
  | n == x = True
  | otherwise = isTruncParent n (l, xs)

truncatedTransDetails :: (Show a, Integral a) => Vr a -> [Integer] -> [([Integer], [Integer])] -> Integer -> Text -> ([RewNodeClust], [RewEdge])
truncatedTransDetails vr sn []   n lbl = (t1, t2)
                        where (t1, t2) = makeCluster ([(sn ++ [n],GreenDiamond lbl)] ++ [(sn ++ [n] ++ [1], GreenBox $ pack $ show vr ++ "_" ++ show sn)],[(sn ++ [n] ++ [1],sn ++ [n],"")]) [0]
truncatedTransDetails vr sn pars n lbl = (fst (makeCluster (pred,[]) sn) ++ fst (makeCluster (trans,[]) [0]), edges)
                        where par = fst (pars !! 0)
                              pid = par ++ [n]
                              pred = [(pid, GreenBox $ pack $ show vr)]-- ++ "_" ++ show (hash sn))]
                              trans = [(pid ++ [1],GreenDiamond lbl)] ++ [(pid ++ [1] ++ [1], WhiteBox "...")]
                              edges = [(par, pid, "")] ++ [(pid, pid ++ [1]  , "")] ++ [(pid ++ [1], pid ++ [1] ++ [1], "")]

--renderTrans' sn depD depR n rt (Trans p _ vr gc dt) = (
getTransDetails :: (Show a, Show b, Show c, Eq a, Eq b, Eq c, Ord c) => [Integer] -> Int -> Int -> Integer -> RewTree a b c Integer -> [RewNode] -> [([Integer], [Integer])] -> Trans a b c Integer -> ([RewNodeClust], [RewEdge])
getTransDetails sn depD depR n rt rewNodes trunc (Trans p _ vr@(Vr v) gc dt) = 
      case findParent vr rewNodes of 
         Nothing -> (tNodes, tEdges)
                        where (tNodes, tEdges) = truncatedTransDetails vr sn parents n lbl
                              parents = filter (isTruncParent (toInteger v)) trunc
                              lbl = pack $ "gc: {" ++ (f $ guardingContext p rt gc) ++ "}" 
			            ++ ", cp: {" ++ (f $ clauseProj p gc) ++ "}"
                              f a = concatMap g a
                              g ((ix, t, v)) = "( " ++ show ix ++ ", " ++ ppTerm t ++ ", " ++ show v ++ "),"
         Just (x,_)  -> (tNodes ++ nodes, tEdges ++ edges)
                        where (nodes, edges) = getDerDetails depD depR (x ++ [n]) 1 dt
                              (tNodes, tEdges) = makeCluster ([(x ++ [n], GreenDiamond lbl)],[(x,x ++ [n], "")] ++ [(x ++ [n], x ++ [n] ++ [1], "")]) [0]
                              lbl = pack $ "gc: {" ++ (f $ guardingContext p rt gc) ++ "}" 
			            ++ ", cp: {" ++ (f $ clauseProj p gc) ++ "}"
                              f a = concatMap g a
                              g ((ix, t, v)) = "( " ++ show ix ++ ", " ++ ppTerm t ++ ", " ++ show v ++ "),"

findParent :: Vr Integer -> [RewNode] -> Maybe RewNode
findParent _  []  = Nothing
findParent (Vr sn) rns = case filter (isParent sn) rns of
                      [] -> Nothing
                      (x:xs) -> Just x

isParent :: Integer -> RewNode -> Bool
isParent sn rn
  | fst rn == []        = False
  | last (fst rn) == sn = True
  | otherwise           = False
