-- | 
-- * Basic program datatypes
--
module CoALP.Program (
	  Program
	, Clause(..)
	, Term(..)
	, Program1
	, Clause1
	, Term1
	, Ident
	, Variable
	, Constant
) where

-- | Type of term for any type of functional symbol and any type of variable.
-- TODO decide which fields should be strict
data Term a b c
	= Var b              -- ^ a variable
	| Const c            -- ^ a integral constant
	| Fun a [Term a b c]   -- ^ a function
	deriving (Eq, Ord)

-- | Type of clause
data Clause a b c = Clause (Term a b c) [Term a b c]

-- | Type of Program
type Program a b c = [Clause a b c]

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

-- | Type of program of first-order term.
--
type Program1 = Program Ident Variable Constant