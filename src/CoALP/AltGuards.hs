module CoALP.AltGuards (
	  gc4one
	, displayRDT
	, constructRew
	) where

import Debug.Trace

import Data.Foldable (asum)
import Data.Traversable (sequenceA,traverse)
import System.Process (spawnCommand)

import CoALP.Parser.PrettyPrint (
	  ppTerm
	, ppClause
	, ppSubst
	, ppProgram
	)

import CoALP.Program (
	  Term (..)
	, Term1
	, Clause (..)
	, Clause1
	, Vr (..)
	, Program
	, Program1
	, Ident
	, Var
	, Constant
	, VR
	, Subst
	)

import CoALP.FreshVar (
	  FreshVar(..)
	, evalFresh
	, initFresh
	, getFresh
	, Freshable (..)
	)

import CoALP.DerTree (
	  clauseProj
	)

import CoALP.Unify (
	  match
	, unify
	, applySubst
	, stripVars
	, composeSubst
	)

import CoALP.Guards (
	recGuardedTerm
	)
-- -------------------- GC4 --------------------------
--
gc4one :: Program1 -> Clause1 -> Bool
gc4one _ _ = False





-- | RDTree - combination of rewriting and derivation (rose) tree

data RDTree a b c d = RDTree (ClauseNode a b c d)


data ClauseNode a b c d
	= ClauseNode (Maybe Int) (Clause a b c) [TermNode a b c d]
--	| ClauseNodeEmpty d

data TermNode a b c d = TermNode (Maybe Int) Int (Term a b c) [RDTrans a b c d]

data RDTrans a b c d 
	= InternalRes (Maybe Int) Int (ClauseNode a b c d)
	| NullRes (Maybe Int) Int (Vr d)
	| UnguardedRes (Maybe Int) Int (Int, Int, Term a b c, Term a b c)
	| ExternalRes (Maybe Int) Int (Subst a b c) [(Int, Term a b c, [Int])] (ClauseNode a b c d)


type TermStack a b c = [(Int, Int, Term a b c)]

type RDTree1 = RDTree Ident Constant Var VR


data CNZipper a b c d =
	CNZipper (Maybe (TransZipper a b c d)) ([TermNode a b c d] -> ClauseNode a b c d) [TermNode a b c d] [TermNode a b c d]

data TNZipper a b c d =
	TNZipper (CNZipper a b c d) ([RDTrans a b c d] -> TermNode a b c d) [RDTrans a b c d] [RDTrans a b c d]

data TransZipper a b c d
	= TransZipper (TNZipper a b c d) (ClauseNode a b c d -> RDTrans a b c d)
	| TransFocus (TNZipper a b c d) (RDTrans a b c d)



	




-- | Construct rewriting tree from a clause
--
constructRew ::
	Program1
	-> Clause1
	-> RDTree1
constructRew p c = rt -- traceShow (findUnif p rt) rt
	where
		rt = 
			expandMatchRew p'' $
			expandMatchRew p' $
			expandMatchRew p $ RDTree (constructCN p Nothing c initFresh)
		p' = map (fmap apartL) p
		p'' = map (fmap apartL) p'


--constructCN ::
--	Program a b c
--	-> ClauseNode a b c d
constructCN p mck c@(Clause _ bs) v = flip evalFresh v $ do -- TODO applicative
		tns <- sequenceA $ zipWith mkTrm bs [0..]
		return $ ClauseNode mck c tns
	where
		mkTrm t tk = do
			trs <- sequenceA $ fmap (f tk) [0..(length p - 1)]
			return $ TermNode mck tk t $ trs
		f tk ck = getFresh >>= \x -> return $ NullRes (Just ck) tk (Vr x)


expandMatchRew ::
	Program1
	-> RDTree1
	-> RDTree1
expandMatchRew p (RDTree cn) = RDTree $ expandCN p [] cn


expandCN p pth (ClauseNode mck c tns)	= ClauseNode mck c $ fmap (expandTN p pth) tns
expandTN p pth (TermNode mck tk t trs) 	= TermNode mck tk t $ zipWith (expandTrs p pth' t) trs p
	where
		pth' = case mck of
			Nothing	-> pth
			Just ck	-> (ck, tk, t):pth

expandTrs p pth t (InternalRes mck tk cn) _		= InternalRes mck tk $ expandCN p pth cn
expandTrs p pth t (ExternalRes mck tk si cp cn) _	= ExternalRes mck tk si cp $ expandCN p pth cn

expandTrs p pth t tr@(NullRes mck tk (Vr v)) (Clause h bs)	= 
		case h `match` t of
			Just th	-> let thbs = map (applySubst th) bs 
				in case findUnguardedLoop pth of
					Nothing ->
						InternalRes mck tk $ 
							expandCN p pth $ 
							constructCN p mck (Clause t thbs) v
					Just l	-> -- traceShow (pth) $ 
						UnguardedRes mck tk l
			Nothing -> case h' `unify` t' of
				Just th	-> ExternalRes mck tk th (mkCP th) $
					constructCN p' mck (mkC th) v
				Nothing	-> tr
	where
		h' = fmap apartR h
		bs' = map (fmap apartR) bs
		p' = map (fmap apartL) p
		t' = fmap apartL t

		mkC si = Clause (si `applySubst` t') $ fmap (si `applySubst`) bs'
		mkCP si = clauseProj p' $ fmap (\x -> (x, si, t')) mck



				
findUnguardedLoop [] = Nothing
findUnguardedLoop ((ck, tk, t):pth) = firstJ $ map (isUng (ck, tk, t)) pth
	where
		firstJ []		= Nothing
		firstJ (Nothing:xs)	= firstJ xs
		firstJ (j:_)		= j
		isUng (ck1, tk1, t1) (ck2, tk2, t2)	= if ck1 == ck2 && tk1 == tk2
			then case recGuardedTerm t2 t1 of
				Just _	-> Nothing
				_	-> Just (ck1, tk1, t1, t2)
			else Nothing


{-
zoomCN pz (ClauseNode mck tk tns) = CNZipper (Just pz)


zoomTrs pz (InternalRes mck tk cn) = zoomCN (TransZipper pz (InternalRes mck tk)) cn
-}

getCNVrs (ClauseNode _ _ tns) = concatMap getTNVrs tns
getTNVrs (TermNode _ _ _ trs) = concatMap getTrsVrs trs
getTrsVrs (InternalRes _ _ cn) = getCNVrs cn
getTrsVrs (NullRes _ _ d) = [d]
getTrsVrs (UnguardedRes _ _ cn) = []

findUnif p (RDTree cn) = findUnifCN p cn

findUnifCN p (ClauseNode _ _ tns) = asum $ map (findUnifTN p) tns
findUnifTN p (TermNode _ _ t trs) = asum $ zipWith (findUnifTrs p t) trs p
findUnifTrs p t (InternalRes _ _ cn) _ = findUnifCN p cn
findUnifTrs p t (NullRes _ _ e) (Clause h bs) = h `unify` t
findUnifTrs p t (UnguardedRes _ _ _) _ = Nothing
findUnifTrs p t (ExternalRes _ _ th _ cn) _ = findUnifCN p cn




displayRDT rdt = do
	writeFile "/tmp/altest.dot" $ renderRDT rdt
	_ <- spawnCommand "dot -Tpng /tmp/altest.dot | display"
	return ()

renderRDT (RDTree (ClauseNode mck c tns)) =
		"digraph G {\n" ++
		"node [];\n" ++
		"0 [shape=box,fixedsize=false,label=\"" ++ lbl ++ "\"]\n" ++
		concat (zipWith (renderTN 0) is tns) ++
		"}\n"
	where
		is = [1..]
		lbl = (sMck mck) ++ ppClause c 

renderCN n n' (ClauseNode mck c tns) = 
		show n' ++ " [shape=box,label=\"" ++ lbl ++ "\"]\n" ++
		show n ++ " -> " ++ (show n') ++ "\n" ++
		concat (zipWith (renderTN n') is tns) ++
		"\n"
	where
		is = [10 * n' + i | i <- [1..]]
		lbl = (sMck mck) ++ ppClause c

renderTN n n' (TermNode mck tk t trs) =
		show n' ++ "[shape=box,label=\"" ++ lbl ++ "\"]\n" ++
		show n ++ " -> " ++ show n' ++ "\n" ++
		concat (zipWith (renderTRS n') is trs) ++
		"\n"
	where
		is = [10 * n' + i | i <- [1..]]
		lbl = sMckTk mck tk ++ ppTerm t


renderTRS n n' (InternalRes mck tk cn) =
		show n' ++ "[shape=diamond,label=\"" ++ lbl ++  "\"]\n" ++
		show n ++ " -> " ++ show n' ++ "\n" ++
		renderCN n' n'' cn ++
		"\n"
	where
		n'' = 10 * n'
		lbl = sMckTk mck tk ++ " match" 

renderTRS n n' (NullRes mck tk vrd) =
		show n' ++ "[shape=diamond,label=\"" ++ lbl ++ "\"]\n" ++
		show n ++ " -> " ++ show n' ++ "\n"
	where
		lbl = -- (sMckTk mck tk)
			"_|_ " ++ show vrd

renderTRS n n' (UnguardedRes mck tk (l, k, t1, t2)) =
		show n' ++ "[shape=diamond,label=\"" ++ lbl ++ "\"]\n" ++
		show n ++ " -> " ++ show n' ++ "\n"
	where
		lbl = sMckTk mck tk ++ "Ung: (" ++ show l ++ ") " ++ ppTerm t1 ++ " -> (" ++ show k ++ ") " ++ ppTerm t2

renderTRS n n' (ExternalRes mck tk th cp cn) =
		show n' ++ "[shape=diamond,label=\"" ++ lbl ++ "\"]\n" ++
		show n ++ " -> " ++ show n' ++ "\n" ++
		renderCN n' n'' cn ++
		"\n"
	where
		n'' = 10 * n'
		lbl = "Ext: (" ++ ppSubst th ++ " " ++ f cp
		f a = concatMap g a
		g ((ix, t, v)) = "( " ++ show ix ++ ", " ++ ppTerm t ++ ", " ++ show v ++ "),"



sMck Nothing = "query) "
sMck (Just i) = show i ++ ") "

sMckTk Nothing i = "-." ++ show i ++ ") "
sMckTk (Just i) j = show j ++ "." ++ show i ++ ") "


