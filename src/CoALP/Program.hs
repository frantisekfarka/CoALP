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
) where

import Data.List (intersperse)
import Numeric (showHex,showIntAtBase)

-- | Type of term for any type of functional symbol and any type of variable.
-- TODO decide which fields should be strict
data Term a b c where
	Var :: Eq b => b -> Term a b c              -- ^ a variable
	Fun :: Eq a => a -> [Term a b c] -> Term a b c   -- ^ a function
--	| Const c            -- ^ a integral constant -- keep out for now

--	deriving (Eq, Ord)
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

-- | Type of Program
type Program a b c = [Clause a b c]


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

data RewTree a b c d = RT (Query a b c) (Subst a b c) [AndNode (Clause a b c) (Term a b c) (Vr d)]
type RewTree1 = RewTree Ident Variable Constant VariableRew

--AndNode (Term a b c)

--type RewTree1 = RewTree Ident Variable Constant

-- | Just Helper
-- TOOD: remove
mkVar :: VariableRew -> Vr VariableRew
mkVar = Vr

