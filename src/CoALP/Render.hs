-- | Render some trees
module CoALP.Render (
	renderProgram,
	displayProgram
) where

--import Data.Foldable
import System.Process

import CoALP.Program (Program1,Clause1, Clause(..),Term1,Term(..))
import CoALP.Parser.PrettyPrint (ppTerm,ppClause,ppTerms)


-- | TODO refactor! -- use tree language!
displayProgram :: Program1 -> IO ()
displayProgram p = do
	saveProgram p "/tmp/test.dot"
	spawnCommand "dot -T svg /tmp/test.dot |  display"
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



--renderTerm h

-- | render in imagemagic dot format
-- TODO refactor
-- render :: ONode Occ -> String
renderTerm :: Int -> Term1 -> String
renderTerm n t0 = (node n t0) ++ (edge n t0)
	where
	--go :: ONode Occ -> Int -> (Int, String)
	node :: Int -> Term1 -> String
	node n (Var i) =
		"\t" ++ show n
		++ "[color=white,label=\"_v" ++ show i ++ "\"];\n"
	node n (Const i) =
		"\t" ++ show n
		++ "[color=white,label=\"" ++ show i ++ "\"];\n"
	node n (Fun f t) =
		"\t" ++ show n ++
		"[shape=box,color=white,width=.2,label=\"" ++ f ++ "\",fixedsize=false];\n" ++
		concat (zipWith node [10*n + i  | i <- [1..]] t)
	edge :: Int -> Term1 -> String
	edge n (Var _) = ""
	edge n (Const _) = ""
	edge n (Fun f t) = 
		concat (zipWith (\m _ -> "\t" ++ show n ++ " -> " ++ show m ++ ";\n") [10*n + i  | i <- [1..]] t) ++
		concat (zipWith edge [10*n + i  | i <- [1..]] t)

	

{-
    go (ONode []) start =
      (start + 1, show start ++
                  "[shape=square,width=.2,label=\"\",fixedsize=true];\n")
    go (ONode ts) start =
      let (next, dot) = connect goA ts (start + 1) start in
      (next, show start ++ " [shape=point];\n" ++ dot)
    goA (ANode occ its) start =
      let (next, dot) = connect go (snd <$> Map.toList its) (start + 1) start in
      (next, show start ++ " [shape=none,label=\"" ++
             show (oTerm occ) ++ "\"];\n" ++ dot)

--    connect :: [ONode Occ] -> Int -> Int -> (Int, String)
    connect fstep ts start parent =
      foldl' (\(start_t, dot) t ->
               let (next, dot_t) = fstep t start_t in
               (next, dot ++ dot_t ++ show parent ++ " -> " ++ show start_t ++
                      "[arrowhead=none];\n"))
             (start, "") ts
-}
