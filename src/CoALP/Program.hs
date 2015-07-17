{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, TypeSynonymInstances  #-}

-- | 
-- Basic program datatypes
--
module CoALP.Program (
	-- * Tier 1 structures
	  Term(..)
	, subtermOf
	, propSubtermOf
	, mapSubst
	, Clause(..)
	--, Query(..)
	, Subst
	, Program

	-- * Tier 2 structures
	, RewTree(..)
	, AndNode(..)
	, OrNode(..)
	, Vr(..)
	, Loop

	-- * Tier 3 structures
	, DerTree(..)
	, Trans(..)
	, GuardingContext
	, OTree(..)
	, OTrans(..)

	-- * Concrete types
	-- ** Identifier, Variable, and Constant 
	, Ident
	, Var
        , AnnoVar(..)
	, Constant
	, VR

	-- ** Datatype synonyms
	, Term1
	, Clause1
	--, Query1
	, Subst1
	, Program1
	, RewTree1
	, Vr1
	, Loop1
	, DerTree1
	, Trans1
	, GuardingContext1
	, OTree1
	, OTrans1
        , TermA
        , ClauseA
        , SubstA
        , ProgramA
) where

import Control.Arrow ((***))
import Data.Bifunctor (Bifunctor(..))
--import Data.Functor(Functor(..))
import Data.Foldable (Foldable,foldMap)
import Data.List (intersperse)
-- import Data.Set (Set)
import Numeric (showHex) -- ,showIntAtBase)

import Control.DeepSeq (deepseq, NFData(..))

-- | A term for any type of functional symbol and any type of variable.
--
-- Term over Σ is a total function from a non-empty tree 
-- to Σ &#x22c3; Var
--
-- Notation:
--
-- * T(ε) is a predicate of term
-- * subterm(t, w) - subtree at a word w
--
data Term a b c
	= Var c			-- ^ a inductive variable
	| Fun a [Term a b c]	-- ^ a function symbol

--	 Const b            -- an integral constant -- keep out for now


instance (Eq a, Eq c) => Eq (Term a b c) where
	(Var v1) == (Var v2) = v1 == v2
	(Fun id1 ts1) == (Fun id2 ts2) = id1 == id2 && ts1 == ts2
	_ == _ = False

instance (Show a, Show b, Show c) => Show (Term a b c) where
	show (Var x) = "_v" ++ show x
	show (Fun f ts) = shows f "(" ++ (concat . intersperse ", " . fmap show $ ts) ++ ")"

instance Functor (Term a b) where
	fmap f (Var v)		= Var $ f v
	fmap f (Fun g ts)	= Fun g $ map (fmap f) ts


instance NFData (Term a b c) where
	rnf (Fun f ts)	= seq f $ deepseq ts $ ()
	rnf (Var v)	= seq v ()
	
-- | A clause 
--
-- Notation: 
--
-- * clause C over Σ is a total function from finite tree language L
--   of depth 1 to terms (term trees)
--
-- * @C(ε)@ is a head of clause
-- * @C(i)@ is a n-th term (for @i =/= ε@)
--
data Clause a b c where
	Clause :: (Term a b c) -> ([Term a b c]) -> Clause a b c

instance (NFData (Term a b c)) => NFData (AndNode (Clause a b c) (Term a b c) (Vr d)) where
	rnf (AndNode t ors) = deepseq t $ deepseq ors $ ()
	

instance (Eq a, Eq c) => Eq (Clause a b c) where
	(Clause h b) == (Clause h' b') = h == h' && b == b'

instance (Show a, Show b, Show c) => Show (Clause a b c) where
	show (Clause h bs) = show h ++ " :- " ++ 
		(concat . intersperse ", " . fmap show $ bs) ++ "."

instance NFData (Clause a b c) where
	rnf (Clause h b) = deepseq h $ deepseq b $ ()

instance Functor (Clause a b) where
	fmap f (Clause h b) = Clause (fmap f h) (fmap (fmap f) b)

-- | A query as a special kind of clause - a goal clause
--
data Query a b c 
	= Query [Term a b c]

instance (Eq a, Eq c) => Eq (Query a b c) where
	(Query b) == (Query b') = b == b'

instance (Show a, Show b, Show c) => Show (Query a b c) where
	show (Query ts ) = "? :- " ++ 
		(concat . intersperse ", " . fmap show $ ts) ++ "."

-- | Substitution on terms
type Subst a b c = [(c, Term a b c)]

-- | map over substitution
-- TODO make subst either newtype or such and this into an instance
mapSubst :: (c -> c') -> [(c, Term a b c)] -> [(c', Term a b c')]
mapSubst f s = map (f *** (fmap f)) s


-- | A program consisting of clauses
--
-- Notation:
--
-- * program P over Σ is total function @{0..n} &#8712; N@ to non-goal clauses
-- * @P(i)@ is n-th clause
--
type Program a b c = [Clause a b c]

-- | GuardingContext gathered for every transition between
-- rewritng trees
type GuardingContext a b c = [(Int, Term a b c, [Int])]


-- | Type of Rew tree Variable
newtype Vr a = Vr { unVr ::  a }

instance (Integral a, Show a) => Show (Vr a) where
	--show x = "Vr_b" ++ showIntAtBase 2 (head.show) (unVr x) ""
	--show x = "Vr_" ++ show (unVr x)
	show x = "Vr_0x" ++ showHex (unVr x) ""

instance Eq a => Eq (Vr a) where
	(Vr x) == (Vr y) = x == y

instance Functor (Vr) where
	fmap f (Vr a) = Vr $ f a

instance NFData (Vr d) where
	rnf (Vr v) = v `seq` ()

-- | Annotated variable
-- needs to derive Eq and Ord
data AnnoVar a
        = Ind a
        | CoInd a 
        deriving (Ord)

instance (Eq a) => Eq (AnnoVar a) where
	(Ind v1) == (Ind v2) = v1 == v2
        (CoInd v1) == (CoInd v2) = v1 == v2
        _ == _ = False

instance (Show a) => Show (AnnoVar a) where
        show (Ind x) = show x ++ "i"
        show (CoInd y) = show y ++ "c"

instance Functor (AnnoVar) where
	fmap f (Ind v)		= Ind $ f v
	fmap f (CoInd c)	= CoInd $ f c


instance NFData (AnnoVar a) where
	rnf (Ind v)	= seq v ()
        rnf (CoInd c)	= seq c ()

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
-- starting from @V_@ followed by the number of that variable. e.g.:
--
-- > nat(s(V_1)) :- nat(V_1)
--
type Var = Integer

type VarA = AnnoVar Integer

-- | Type of Rew Tree Variable
--
type VR = Integer

-- | Type of integral constant
--
-- Integral constants are allowed and represented as Integers
--
type Constant = Integer

-- | Type of first-order term.
--
type Term1 = Term Ident Constant Var

type TermA = Term Ident Constant VarA 

-- | Type of clause of first-order terms.
--
type Clause1 = Clause Ident Constant Var

type ClauseA = Clause Ident Constant VarA 

-- | Type of clause of first-order query
--
--type Query1 = Query Ident Constant Var

-- | Type of program of first-order term.
--
type Program1 = Program Ident Constant Var

type ProgramA = Program Ident Constant VarA 

-- | Type of substitution of terms
--
type Subst1 = Subst Ident Constant Var

type SubstA = Subst Ident Constant VarA 

-- | Rewriting tree for Term1
--
type RewTree1 = RewTree Ident Constant Var VR

type RewTreeA = RewTree Ident Constant VarA VarA 

-- | The derivation tree
--
type DerTree1 = DerTree Ident Constant Var VR

type DerTreeA = DerTree Ident Constant VarA VarA 

-- | Type of rewritng tree variables
--
type Vr1 = Vr VR

type VRA = Vr VarA 

-- | Type of a GC
type GuardingContext1 = GuardingContext Ident Constant Var

type GuardingContextA = GuardingContext Ident Constant VarA 

-- | @AndNode a its@ is an atom with a possibly partial mapping from clauses to
-- or-subtrees. Each of those or-subtrees corresponds to some clause number @i@
-- such that the head of that clause has been unified with @a@ and its unified
-- body atoms label the roots of the and-subtrees of the @i@-th 'OrNode'.
--
-- Or nodes in the list correspond to clauses in program
--
-- This datatype represents the @Definition 3.3 3)@
data AndNode a b c
	= AndNode b [OrNode a b c]
	deriving (Eq)

instance Bifunctor (AndNode (Clause a b c)) where
	first f (AndNode t ors) = AndNode (f t) $ fmap (first f) ors
	second f (AndNode t ors) = AndNode t $ fmap (second f) ors


instance Foldable (AndNode (Clause a b c) (Term a b c)) where
	foldMap f (AndNode _ ors) = foldMap (foldMap f) ors

-- | @OrNode ts@ is the or-subtree corresponding to unification against a clause
-- in the logic program where @ts@ are the trees built by matching against the
-- body terms of that clause.
--
-- A separate case is the topmost 'ONode' which contains the list of _goals_ to
-- be unified against a logic program.
--
-- This datatype represents the @Definition 3.3 2)@
data OrNode a b c
	= OrNode a [AndNode a b c]
	| OrNodeEmpty c
	deriving (Eq)

instance Bifunctor (OrNode (Clause a b c)) where
	first f (OrNode c ands) 	= OrNode ({- fmap f -} c) $ fmap (first f) ands
	first _ (OrNodeEmpty v)		= OrNodeEmpty v
	second f (OrNode c ands) 	= OrNode c $ fmap (second f) ands
	second f (OrNodeEmpty v)	= OrNodeEmpty $ f v

instance Foldable (OrNode (Clause a b c) (Term a b c)) where
	foldMap f (OrNode _ ands) = foldMap (foldMap f) ands
	foldMap f (OrNodeEmpty d) = f d

instance NFData (OrNode (Clause a b c) (Term a b c) (Vr d)) where
	rnf (OrNode c ands) = deepseq c $ deepseq ands $ ()
	rnf (OrNodeEmpty d) = d `seq` ()

-- | Rewriting tree
--
-- This datatype represents the @Definition 3.3@
data RewTree a b c d = RTEmpty | RT (Clause a b c) (Subst a b c) [AndNode (Clause a b c) (Term a b c) (Vr d)]

instance (Show a, Show b, Show c, Show d, Integral d) => Show (RewTree a b c d) where
	show (RTEmpty)		= "Empty Rew Tree"
	show (RT c s ands)	= "RT: " ++ show c ++ " | " ++ show s ++ "\n" ++
			concatMap (showAnd 1) ands
		where
			showAnd n (AndNode b ors) = pref n ++ show b ++ "\n" ++
				concatMap (showOr (n+1)) ors 
			showOr n (OrNodeEmpty c') = pref n ++ show c' ++ "\n"
			showOr n (OrNode a as) = pref n ++ show a ++ "\n" ++
				(if (n < 5) then concatMap (showAnd (n+1)) as else "")
			pref n = take n $ repeat ' '

{-instance Bifunctor (RewTree a b) where
	first f (RT c s ands)	= RT (fmap f c) (mapSubst f s) (fmap (first f) ands)
	first _ RTEmpty		= RTEmpty
	second f (RT c s ands)	= RT c s (fmap (second (Vr . f . unVr)) ands)
	second _ RTEmpty	= RTEmpty
-}

-- | Derivation tree
--
-- This datatype represents @Definition 3.6@
data DerTree a b c d = DT (RewTree a b c d) [Trans a b c d]

-- | Transition between rewriting trees
-- 
-- see @Definition 3.5@
data Trans a b c d = Trans (Program a b c) (RewTree a b c d) (Vr d) (Maybe (Int, Subst a b c, Term a b c)) (DerTree a b c d)

-- | Loop in Rewritng trees
--
-- Captures @Definition 5.2@
type Loop a b c = (Term a b c, Term a b c, Int)

-- | Loop on Term1's
type Loop1 = Loop Ident Var Constant 


-- | Helper datatype for Observation tree
--
-- see @Definition 5.5@
data OTree a b c d = ODT (RewTree a b c d) [OTrans a b c d] | UNRT (RewTree a b c d)

-- | Helper datatype for transition in Observation tree
--Guarded transition carries it's guarding context
--
-- see @Definition 5.5@
data OTrans a b c d 
	= OTrans (Program a b c) (RewTree a b c d) (Vr d) (Maybe (Int, Subst a b c, Term a b c)) (OTree a b c d)
	| GTrans (Vr d) [GuardingContext a b c] (GuardingContext a b c)


-- | Type of Observation tree
type OTree1 = OTree Ident Var Constant VR

-- | Type of Observation trans
type OTrans1 = OTrans Ident Var Constant VR

-- | Type of Derivation tree transition
type Trans1 = Trans Ident Var Constant VR





-- | Test whether first argument is a subterm of second argument
subtermOf :: (Eq a, Eq c) => Term a b c -> Term a b c -> Bool
subtermOf t1 t2@(Var _) = t1 == t2
subtermOf t1 t2@(Fun _ t2ts) = t1 == t2 || any (subtermOf t1) t2ts

-- | Test whether first argument is a /proper/ subterm of second argument
propSubtermOf :: (Eq a, Eq c) => Term a b c -> Term a b c -> Bool
propSubtermOf t1 t2 = t1 /= t2 && t1 `subtermOf` t2



