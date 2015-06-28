{-# LANGUAGE GADTs #-}

-- | 
-- * Basic program datatypes
--
module CoALP.Program (
	  Program
	, Clause(..)
	, Query(..)
	, Term(..)
	, Subst
	, Subst1
	, RewTree(..)
	, RewTree1
	, DerTree(..)
	, DerTree1
	, Program1
	, Clause1
	, Query1
	, Term1
	, Ident
	, Variable
	, Constant
	, AndNode(..)
	, OrNode(..)
	, Vr(..)
	, Vr1
	, mkVar
	, mapVar
	, fixTerm
	, fixQuery
	, Trans(..)
	, Trans1
	, GuardingContext
	, GuardingContext1
	, OTree(..)
	, OTree1
	, OTrans(..)
	, OTrans1
	, mapTerm
	, mapSubst
	, mapClause
	, Loop
	, Loop1
	, subtermof
) where

import Data.List (intersperse)
import Data.Set (Set)
import Numeric (showHex) -- ,showIntAtBase)

-- | Type of term for any type of functional symbol and any type of variable.
-- TODO decide which fields should be strict
data Term a b c where
	Var :: Eq b => b -> Term a b c              -- ^ a variable
	Fun :: Eq a => a -> [Term a b c] -> Term a b c   -- ^ a function
--	| Const c            -- ^ a integral constant -- keep out for now


instance (Eq a, Eq b) => Eq (Term a b c) where
	(Var v1) == (Var v2) = v1 == v2
	(Fun id1 ts1) == (Fun id2 ts2) = id1 == id2 && ts1 == ts2
	_ == _ = False

--
instance (Show a, Show b, Show c) => Show (Term a b c) where
	show (Var x) = "_v" ++ show x
	show (Fun f ts) = shows f "(" ++ (concat . intersperse ", " . map show $ ts) ++ ")"

-- | Type of clause
data Clause a b c where
	Clause :: Term a b c -> [Term a b c] -> Clause a b c

instance (Show a, Show b, Show c) => Show (Clause a b c) where
	show (Clause h bs) = show h ++ " :- " ++ 
		(concat . intersperse ", " . map show $ bs) ++ "."

-- | Type of Query Clause
data Query a b c 
	= Query [Term a b c]

instance (Show a, Show b, Show c) => Show (Query a b c) where
	show (Query ts ) = "? :- " ++ 
		(concat . intersperse ", " . map show $ ts) ++ "."

-- | Type of Program
type Program a b c = [Clause a b c]

-- | GuardingContext type
type GuardingContext a b c = [(Int, Term a b c, [Int])]
--type GuardingContext a b c = Set (Int, Term a b c, [Int])
type GuardingContext1 = GuardingContext Ident Variable Constant

-- | Type of Rew tree Variable
newtype Vr a = Vr { unVr ::  a }

instance (Integral a, Show a) => Show (Vr a) where
	--show x = "Vr_b" ++ showIntAtBase 2 (head.show) (unVr x) ""
	--show x = "Vr_" ++ show (unVr x)
	show x = "Vr_0x" ++ showHex (unVr x) ""

instance Eq a => Eq (Vr a) where
	(Vr x) == (Vr y) = x == y

-- | Type of identifier
--
-- Identifiers start with a lower-case Latin letter or a symbol drawn from a
-- limited set not containing the colon which is reserved to the \"from\"
-- separator @:-@ written exactly like in conventional LP. The rest of the
-- identifier may also contain uppercase Latin letters. Examples of identifiers
-- are
--
-- > tYPE     p     <=     0th
type Ident = String

-- | Type of variable
--
-- Variables are essentially non-negative integers. However, the user interface
-- allows denoting variables with names that start with an upper-case Latin
-- letter followed by any Latin letters, symbols (same as in identifiers) or
-- decimal digits. The parser then converts variable names to variable
-- numbers. When arbitrary terms are printed, each variable is denoted by a name
-- starting from @X_@ followed by the number of that variable.
--
type Variable = Int

-- | Type of Rew Tree Variable
--
-- most likely the same as 'Variable' (or with bigger width?)
type VariableRew = Integer

-- | Type of integral constant
--
-- Integral constants are allowed and represented as Integers
--
type Constant = Integer

-- | Type of first-order term.
--
type Term1 = Term Ident Variable Constant


-- | Type of clause of first-order terms.
--
type Clause1 = Clause Ident Variable Constant


-- | Type of clause of first-order query
--
type Query1 = Query Ident Variable Constant

-- | Type of program of first-order term.
--
type Program1 = Program Ident Variable Constant

-- | Type of program of first-order term.
--
type Vr1 = Vr VariableRew



-- | @AndNode a its@ is an atom with a possibly partial mapping from clauses to
-- or-subtrees. Each of those or-subtrees corresponds to some clause number @i@
-- such that the head of that clause has been unified with @a@ and its unified
-- body atoms label the roots of the and-subtrees of the @i@-th 'OrNode'.
--
-- aka ``Term tree''
-- Or nodes in the list correspond to clauses in program
data AndNode a b c
	= AndNode b [OrNode a b c]
	deriving (Eq)

-- | @ONode ts@ is the or-subtree corresponding to unification against a clause
-- in the logic program where @ts@ are the trees built by matching against the
-- body terms of that clause.
--
-- A separate case is the topmost 'ONode' which contains the list of _goals_ to
-- be unified against a logic program.
--
-- aka ``Clause tree''
data OrNode a b c
	= OrNode a [AndNode a b c]
	| OrNodeEmpty c
	deriving (Eq)


-- | TODO
type Subst a b c = [(b, Term a b c)]
type Subst1 = Subst Ident Variable Constant

data RewTree a b c d = RTEmpty | RT (Clause a b c) (Subst a b c) [AndNode (Clause a b c) (Term a b c) (Vr d)]
type RewTree1 = RewTree Ident Variable Constant VariableRew

instance (Show a, Show b, Show c, Show d, Integral d) => Show (RewTree a b c d) where
	show (RTEmpty)		= "Empty Rew Tree"
	show (RT c s ands)	= "RT: " ++ show c ++ " | " ++ show s ++ "\n" ++
			concatMap (showAnd 1) ands
		where
			showAnd n (AndNode b ors) = pref n ++ show b ++ "\n" ++
				concatMap (showOr (n+1)) ors 
			showOr n (OrNodeEmpty c) = pref n ++ show c ++ "\n"
			showOr n (OrNode a as) = pref n ++ show a ++ "\n" ++
				(if (n < 5) then concatMap (showAnd (n+1)) as else "")
			pref n = take n $ repeat ' '

data DerTree a b c d = DT (RewTree a b c d) [Trans a b c d]
type DerTree1 = DerTree Ident Variable Constant VariableRew

data Trans a b c d = Trans (Program a b c) (Vr d) (Maybe (Int, Subst a b c, Term a b c)) (DerTree a b c d)
type Trans1 = Trans Ident Variable Constant VariableRew 

data OTree a b c d = ODT (RewTree a b c d) [OTrans a b c d] | UNRT (RewTree a b c d)
type OTree1 = OTree Ident Variable Constant VariableRew

data OTrans a b c d = OTrans (Program a b c) (Vr d) (Maybe (Int, Subst a b c, Term a b c)) (OTree a b c d)
	| GTrans (Vr d) [GuardingContext a b c] (GuardingContext a b c)
type OTrans1 = OTrans Ident Variable Constant VariableRew 

type Loop a b c = (Term a b c, Term a b c, Int)
type Loop1 = Loop Ident Variable Constant 

-- | Just Helper
-- TOOD: remove
mkVar :: VariableRew -> Vr VariableRew
mkVar = Vr

mapVar :: (Eq b, Eq b') => (b -> b') -> Term a b c -> Term a b' c
mapVar f (Var v)	= Var $ f v
mapVar f (Fun idn ts)	= Fun idn $ fmap (mapVar f) ts

-- | Renaming hack
-- TODO remove
fixTerm :: Term a Int c -> Term a Int c
fixTerm (Var v) = Var (v + 981)
fixTerm (Fun f ts) = Fun f $ fmap fixTerm ts


fixQuery :: Query1 -> Query1
fixQuery (Query ts) = Query $ fmap fixTerm ts

-- TODO make Term (bi)functor
mapTerm :: (Eq b, Eq b') => (b -> b') -> Term a b c -> Term a b' c
mapTerm f (Fun idn ts) = Fun idn $ map (mapTerm f) ts
mapTerm f (Var a) = Var $ f a

mapSubst :: (Eq b', Eq b) =>
	(b -> b') -> [(b, Term a b c)] -> [(b', Term a b' c)]
mapSubst f s = map oneS s
	where
		oneS (b, t) = (f b, mapTerm f t)

mapClause :: (Eq b', Eq b) =>
	(b -> b') -> Clause a b c -> Clause a b' c
mapClause f (Clause h b) = Clause (mapTerm f h) (map (mapTerm f) b)


-- is subterm of
subtermof :: (Eq a, Eq b) => Term a b c -> Term a b c -> Bool
subtermof t1 t2@(Var _) = t1 == t2
subtermof t1 t2@(Fun _ t2ts) = t1 == t2 || any (subtermof t1) t2ts

