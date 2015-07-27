-- | CoALP parser pretty printing helpers
module CoALP.Parser.PrettyPrint (
	  ppTerm
	, ppClause
	, ppProgram
	, ppSubst
--        , ppSubstitutions
)where

import Data.List (intersperse)

import CoALP.Program (
	  Program
	, Clause(..)
	, Term(..)
	,Subst
	)

-- | Program pretty printer
ppProgram :: (Show a, Show b, Show c) => Program a b c -> String
ppProgram = concat . intersperse "\n" . ppLines . (map ppClause) 
	where
	ppLines = zipWith (\x y -> show x ++ ":\t" ++ y) ([1..] :: [Integer])

-- | Clause pretty printer
ppClause :: (Show a, Show b, Show c) => Clause a b c -> String
ppClause (Clause h (bs@(_:_))) = ppTerm h ++ " :- " ++ ppTerms bs ++ "."
ppClause (Clause h []) = ppTerm h ++ "."

-- | Term list pretty printer
ppTerms :: (Show a, Show b, Show c) =>  [Term a b c] -> String
ppTerms ts = concat . intersperse ", " . map ppTerm $ ts

-- | Term pretty printer
ppTerm :: (Show a, Show b, Show c) => Term a b c -> String
ppTerm (Var x) = "V_" ++ show x
ppTerm (Fun f ts) = (filter (/= '"') $ show f) ++
	if null ts 
		then ""
		else "(" ++ ppTerms ts ++ ")"
--ppTerm (Const i) = show i

-- | Substitution pretty printer
ppSubst :: (Show a, Show b, Show c) => Subst a b c -> String
ppSubst s = "{ " ++ (concat . intersperse ", " . map f) s ++ " }"
	where
		f (v,t) = ppTerm t ++ " / V_" ++ show v


--ppSubstitutions :: (Show a, Show b, Show c) => [((Term a b c, Term a b c), c)] -> String
--ppSubstitutions s = "{ " ++ (concat . intersperse ", " . map f) s ++ " }"
--       where f ((t1,t2), v) = "(" ++ ppTerm t1 ++ " / V_" ++ show v ++ ", " ++ ppTerm t2 ++ " / V_" ++ show v ++ ")"


