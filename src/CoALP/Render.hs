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
import Data.GraphViz.Attributes.Complete--(Attribute(..), StyleName(..), StyleItem(..), PortName(..), RecordField(..), Label(..))
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

type TermNode = ([Integer], StyledTerm)
type TermEdge = ([Integer], [Integer], String)
--type TermNodeClust = ([Integer], Cluster String StyledTerm)
data StyledTerm = VarTerm Text | FunTerm Text | RRecord Text

type RewNode = ([Integer], StyledText)
type RewEdge = ([Integer], [Integer], String)
type RewNodeClust = ([Integer], Cluster [Integer] StyledText)
data StyledText = BlueBox Text | GreenBox Text | WhiteBox Text | GreenDiamond Text | EmptyText

data Cluster a b  = Cluster a b

-- | Display the whole program
displayProgram :: (Show b, Show c, Ord b, Ord c) => Program String b c -> IO ()
displayProgram p = do
	saveProgram p "/tmp/test.dot"
	_ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
	return ()

-- | Display rewriting tree up to depth
displayRewTree :: (Show a, Show b, Show c, Show d, Integral d) => Int -> RewTree a b c d -> IO ()
displayRewTree depth rt = do
	saveRewT depth rt "/tmp/test.dot"
	_ <- spawnCommand "dot -T svg /tmp/test.dot |  display"
	return ()
	
-- | Display derivation tree up to depthD, render rewriting trees in nodeѕ up to depthR
displayDerTree :: (Show a, Show b, Show c, Eq a, Eq b, Ord c) => Int -> Int -> DerTree a b c Integer -> IO ()
displayDerTree depD depR dt = -- trace "Display der tree ... " $ 
	do
                saveDerT depD depR dt "/tmp/test.dot"
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

saveProgram :: (Show a, Show b, Show c) => Program a b c -> FilePath -> IO ()
saveProgram p f = do
                _ <- runGraphviz (renderProgram p) Canon f 
                return ()

termStyle :: (t, StyledTerm) -> [Attribute]
termStyle (_, VarTerm l) = [textLabel l, color White, FontName "Monospace"]
termStyle (_, FunTerm l) = [textLabel l, shape BoxShape, color White, FontName "Monospace"]
termStyle (_, RRecord l) = [textLabel l, shape Record, color Red, FontName "Monospace"] -- TODO Use ports and record labels instead

-- | Render Clauses in program
renderProgram :: (Show c, Show b, Show a) => [Clause a b c] -> DotGraph [Integer]
renderProgram p = graphElemsToDot params nodes edges
       where idxp = zip [1..] p
             details = map clauseDetails idxp
             nodes = concat $ map fst details
             edges = concat $ map snd details
             params = defaultParams { clusterBy = clustBy
                                    , isDotCluster = isTrueCluster
                                    , clusterID = Str
                                    , fmtCluster = cStyle
                                    , fmtNode = termStyle }
             cStyle l = [ GraphAttrs [style (SItem Dashed []), color Gray, textLabel l, FontName "Monospace"] ]
             clustBy (n, Cluster i l) = C (pack $ i) (N (n, l))

-- | Get nodes and edges of a single clause
clauseDetails :: (Show c, Show b, Show a) => (Integer, Clause a b c) -> ([([Integer], Cluster String StyledTerm)], [TermEdge])
clauseDetails (n, c@(Clause h b)) = makeCluster (startNodes ++ nodes,edges) helper
       where 
       -- TODO fix to use records, in the meantime order matters here 
       startNodes = [([n] ++ [3], RRecord (pack " _ ")), ([n] ++ [2], RRecord (pack " :- ")), ([n] ++ [1], RRecord (pack " _ "))] 
       headDetails = zipWith (termDetails ([n] ++ [1])) [1..] [h]
       bodyDetails = zipWith (termDetails ([n] ++ [3])) [1..] b
       nodes = (concat $ map fst headDetails) ++ (concat $ map fst bodyDetails)
       edges = (concat $ map snd headDetails) ++ (concat $ map snd bodyDetails)
       helper = if null b 
		then ppClause c
		else ppTerm h ++ " :- ..."

-- | Get nodes and edges of a single Term
termDetails :: (Show a, Show b, Show c) => [Integer] -> Integer -> Term a b c -> ([TermNode],[TermEdge])
termDetails par n v@(Var _) = ([(par ++ [n], VarTerm $ pack (ppTerm v))], [(par, par ++ [n], "")])
termDetails par n (Fun f ts) = ([(par ++ [n], (FunTerm $ pack $ show f))] ++ nodes, [(par, par ++ [n], "")] ++ edges)
        where nodes = concat $ map fst children
              edges = concat $ map snd children
              children = zipWith (termDetails (par ++ [n])) [1..] ts


saveRewT :: (Show a, Show b, Show c, Show d, Integral d) => Int -> RewTree a b c d -> FilePath -> IO ()
saveRewT depth rt f = do
              _ <- runGraphviz (renderRewTree depth rt) Canon f
              return ()
renderRewTree :: (Show d, Show c, Show b, Show a, Integral d) => Int -> RewTree a b c d -> DotGraph [Integer]
renderRewTree depth rt = graphElemsToDot params nodes edges
       where (nodes, edges) = rewTDetails depth [1] rt 1
             params = nonClusteredParams { globalAttributes = ga, fmtNode = nStyle }
             ga = [ GraphAttrs [] ]

nStyle :: (t, StyledText) -> [Attribute]
nStyle (_, BlueBox l)  = [textLabel l, shape BoxShape, color Blue, FontName "Monospace"]
nStyle (_, GreenBox l) = [textLabel l, shape BoxShape, color Green, FontName "Monospace"]
nStyle (_, WhiteBox l) = [textLabel l, shape BoxShape, color White, FontName "Monospace"]
nStyle (_, GreenDiamond l) = [textLabel l, shape MDiamond, color Green, FontName "Monospace"]
nStyle (_, EmptyText) = [textLabel "", FontName "Monospace"]

-- | Get nodes and edges of a rewriting tree
rewTDetails :: (Show a, Show b, Show c, Show d, Integral d) => Int -> [Integer] -> RewTree a b c d -> Integer -> ([RewNode], [RewEdge])
rewTDetails _ par RTEmpty n = ([(par ++ [n], BlueBox $ pack "_|_")],[])
rewTDetails depth par (RT q s os) n = ([(par ++ [n], BlueBox $ pack $ ppClause q ++ " | " ++ ppSubst s)] ++ nodes, edges)
        where nodes = concat $ map fst children
              edges = concat $ map snd children
              children = zipWith (rewAndDetails par (par ++ [n]) (depth-1)) [1..] os

rewTDetails' :: (Show a, Show b, Show c, Show d, Integral d) => Int -> [Integer] -> RewTree a b c d -> Integer -> ([RewNode], [RewEdge])
rewTDetails' _ par RTEmpty n = ([(par ++ [n], BlueBox $ pack "_|_")],[])
rewTDetails' depth par (RT q s os) n = ([(par ++ [n], BlueBox $ pack $ "unguarded\\n" ++ ppClause q ++ " | " ++ ppSubst s)] ++ nodes, edges)
        where nodes = concat $ map fst children
              edges = concat $ map snd children
              children = zipWith (rewAndDetails par (par ++ [n]) (depth-1)) [1..] os

-- | Get Nodes and edes of a And Node
rewAndDetails :: (Show a, Show b, Show c, Show d, Integral d) => [Integer] -> [Integer] -> Int -> Integer -> AndNode (Clause a b c) (Term a b c) (Vr d) -> ([RewNode], [RewEdge])
rewAndDetails _ par 0 n _ = ([(par ++ [n], WhiteBox $ pack "...")],[(par, par ++ [n], "")])
rewAndDetails sn par depth n (AndNode t ors) = ([(par ++ [n], WhiteBox $ pack $ ppTerm t)] ++ nodes, [(par, par ++ [n], "")] ++ edges)
        where nodes = concat $ map fst children
              edges = concat $ map snd children
              children = zipWith (rewOrDetails sn (par ++ [n]) (depth - 1)) [1..] ors

-- | Get Nodes and edes of a Or Node
rewOrDetails :: (Show a, Show b, Show c, Show d, Integral d) => [Integer] -> [Integer] -> Int -> Integer -> OrNode (Clause a b c) (Term a b c) (Vr d) -> ([RewNode], [RewEdge])
rewOrDetails _sn par 0 n _ = ([(par ++ [n], WhiteBox $ pack "...")], [(par, par ++ [n], "")])
rewOrDetails _sn par _depth _n (OrNodeEmpty x@(Vr v)) = ([(par ++ [(toInteger v)], GreenBox $ pack $ show x )], [(par, par ++ [(toInteger v)], "")])
rewOrDetails sn par depth n (OrNode c ands) = ([(par ++ [n], WhiteBox $ pack $ ppClause c)] ++ nodes, [(par, par ++ [n], "")] ++ edges)
        where nodes = concat $ map fst children
              edges = concat $ map snd children
              children = zipWith (rewAndDetails sn (par ++ [n]) (depth -1)) [1..] ands

saveDerT :: (Show a, Show b, Show c, Eq a, Eq b, Ord c) => Int -> Int -> DerTree a b c Integer -> FilePath -> IO ()
saveDerT depD depR dt f = do
              _ <- runGraphviz (renderDerTree depD depR dt) Canon f
              return ()

renderDerTree :: (Show c, Show b, Show a, Ord c, Eq b, Eq a) => Int -> Int -> DerTree a b c Integer -> DotGraph [Integer]
renderDerTree depD depR dt = graphElemsToDot params nodes edges
       where (nodes, edges) = derDetails depD depR [1] 1 dt
             params = defaultParams { globalAttributes = ga
                                     , clusterBy = clustByRewT
                                     , isDotCluster = isTrueCluster
                                     , clusterID = Str
                                     , fmtCluster = cStyle
                                     , fmtNode = nStyle }
             ga = [ GraphAttrs [] ] 
             cStyle _ = [ GraphAttrs [style (SItem Dashed []), color Gray] ]
             clustByRewT (n, Cluster i l) = C (pack $ show $ hash i) (N (n, l))

-- | Get nodes and edges of a derivation tree
derDetails :: (Show a, Show b, Show c, Eq a, Eq b, Eq c, Ord c) => Int -> Int -> [Integer] -> Integer -> DerTree a b c Integer -> ([RewNodeClust], [RewEdge])
derDetails depD depR par n (DT rt trans) = case gcRewTree rt of
        False -> (clNodes, rtEdges)-- Unguarded
        True  -> (clNodes ++ tNodes, rtEdges ++ tEdges)
        where (clNodes, _) = makeCluster ns par
              ns@(rtNodes, rtEdges) = rewTDetails depR par rt n
              (tNodes, tEdges) = (concat $ map fst td, concat $ map snd td )
              td = (zipWith (\x -> transDetails par (depD - 1) depR x rt rtNodes truncation) [1..] trans)
              truncation = rewTTrunc depR par rt n -- [([Integer], [Integer])]

-- | Get indexes of truncated children and their parent within a rewriting tree of a set depth
rewTTrunc :: (Show a, Show b, Show c, Show d, Integral d) => Int -> [Integer] -> RewTree a b c d -> Integer -> [([Integer], [Integer])]
rewTTrunc _ _par RTEmpty _n = []
rewTTrunc depth par (RT _q _s os) n = concat $ zipWith (rewAndTrunc par (par ++ [n]) (depth-1)) [1..] os

-- | Get indexes of truncated children and their parent of an And node
rewAndTrunc ::  (Show a, Show b, Show c, Show d, Integral d) => [Integer] -> [Integer] -> Int -> Integer -> AndNode (Clause a b c) (Term a b c) (Vr d) -> [([Integer], [Integer])]
rewAndTrunc _sn par 0 n (AndNode _t ors) = [(par ++ [n], varsTrunc)]
        where varsTrunc = concat $ map rewOrVars ors
rewAndTrunc sn par depth n (AndNode _t ors) = concat $ zipWith (rewOrTrunc sn (par ++ [n]) (depth - 1)) [1..] ors

-- | Get indexes of truncated children and their parent of an Or node
rewOrTrunc :: (Show a, Show b, Show c, Show d, Integral d) => [Integer] -> [Integer] -> Int -> Integer -> OrNode (Clause a b c) (Term a b c) (Vr d) -> [([Integer], [Integer])]
rewOrTrunc _sn par 0 n ors = [(par ++ [n], varsTrunc)]
        where varsTrunc = rewOrVars ors
rewOrTrunc _sn _par _depth _n (OrNodeEmpty _) = []
rewOrTrunc sn par depth n (OrNode _c ands) = concat $ zipWith (rewAndTrunc sn (par ++ [n]) (depth -1)) [1..] ands

-- | Get all possible indexes of an and node        
rewAndVars :: (Integral d) => AndNode (Clause a b c) (Term a b c) (Vr d) -> [Integer]
rewAndVars (AndNode _t ors) = concat $ map rewOrVars ors

-- | Get all possible indexes of an or node
rewOrVars :: (Integral d) => OrNode (Clause a b c) (Term a b c) (Vr d) -> [Integer]
rewOrVars (OrNodeEmpty (Vr v)) = [toInteger v]
rewOrVars (OrNode _c ands) = concat $ map rewAndVars ands

-- | Get nodes and edges of a transition
transDetails :: (Show a, Show b, Show c, Eq a, Eq b, Eq c, Ord c) => [Integer] -> Int -> Int -> Integer -> RewTree a b c Integer -> [RewNode] -> [([Integer], [Integer])] -> Trans a b c Integer -> ([RewNodeClust], [RewEdge])
transDetails sn depD depR n rt rewNodes trunc (Trans p _ vr@(Vr v) gc dt) = 
      case findParent vr rewNodes of 
         Nothing -> (tNodes, tEdges)
                        where (tNodes, tEdges) = transDetailsTrunc vr sn parents n lbl
                              parents = filter (isTruncParent (toInteger v)) trunc
                              lbl = pack $ "gc: {" ++ (f $ guardingContext p rt gc) ++ "}" 
			            ++ ", cp: {" ++ (f $ clauseProj p gc) ++ "}"
                              f a = concatMap g a
                              g ((ix, t, va)) = "( " ++ show ix ++ ", " ++ ppTerm t ++ ", " ++ show va ++ "),"
         Just (x,_)  -> (tNodes ++ nodes, tEdges ++ edges)
                        where (nodes, edges) = derDetails depD depR (x ++ [n]) 1 dt
                              (tNodes, tEdges) = makeCluster ([(x ++ [n], GreenDiamond lbl)],[(x,x ++ [n], "")] ++ [(x ++ [n], x ++ [n] ++ [1], "")]) [0]
                              lbl = pack $ "gc: {" ++ (f $ guardingContext p rt gc) ++ "}" 
			            ++ ", cp: {" ++ (f $ clauseProj p gc) ++ "}"
                              f a = concatMap g a
                              g ((ix, t, va)) = "( " ++ show ix ++ ", " ++ ppTerm t ++ ", " ++ show va ++ "),"

-- | Get nodes and edges of transition attached to a truncated rewriting tree
transDetailsTrunc :: (Show a, Integral a) => Vr a -> [Integer] -> [([Integer], [Integer])] -> Integer -> Text -> ([RewNodeClust], [RewEdge])
transDetailsTrunc vr sn []   n lbl = (t1, t2)
                        where (t1, t2) = makeCluster ([(sn ++ [n],GreenDiamond lbl)] ++ [(sn ++ [n] ++ [1], GreenBox $ pack $ show vr ++ "_" ++ show sn)],[(sn ++ [n] ++ [1],sn ++ [n],"")]) [0]
transDetailsTrunc vr sn pars n lbl = (fst (makeCluster (prev,[]) sn) ++ fst (makeCluster (trans,[]) [0]), edges)
                        where par = fst (pars !! 0)
                              pid = par ++ [n]
                              prev = [(pid, GreenBox $ pack $ show vr)]-- ++ "_" ++ show (hash sn))]
                              trans = [(pid ++ [1],GreenDiamond lbl)] ++ [(pid ++ [1] ++ [1], WhiteBox "...")]
                              edges = [(par, pid, "")] ++ [(pid, pid ++ [1]  , "")] ++ [(pid ++ [1], pid ++ [1] ++ [1], "")]

-- Find the trucated node which should be the parent of this node
isTruncParent :: Integer -> ([Integer], [Integer]) -> Bool
isTruncParent _ (_, []) = False
isTruncParent n (l,(x:xs)) 
  | n == x = True
  | otherwise = isTruncParent n (l, xs)

-- Find the parent node
findParent :: Vr Integer -> [RewNode] -> Maybe RewNode
findParent _  []  = Nothing
findParent (Vr sn) rns = case filter (isParent sn) rns of
                      [] -> Nothing
                      (x:_) -> Just x

-- Is this node a parent
isParent :: Integer -> RewNode -> Bool
isParent sn rn
  | fst rn == []        = False
  | last (fst rn) == sn = True
  | otherwise           = False

-- Check if a cluster is a graphviz cluster
isTrueCluster :: Text -> Bool
isTrueCluster cl
  | cl == (pack $ show $ hash ([0] :: [Int]) ) = False
  | otherwise  = True

-- Make the node into a cluster
makeCluster :: ([(a, b)], c) -> d -> ([(a, Cluster d b)], c)
makeCluster (ns, es) c = (clustered,es)
        where clustered = map (\(n, l) -> (n, Cluster c l)) ns

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

