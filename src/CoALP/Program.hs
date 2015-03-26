-- | Main program datatypes
-- TODO - refactor from 
module CoALP.Program (
	  Program
	, Clause(..)
	, Term(..)
) where

type Program = [Clause]

data Clause = Clause Term [Term] 
	deriving Show

data Term
	= Var String
	| Fun String [Term]
	| Const Int
	deriving Show


