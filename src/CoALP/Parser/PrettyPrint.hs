-- | CoALP parser pretty printing helpers
module CoALP.Parser.PrettyPrint 
where

import Data.List (intersperse)


-- TODO refactor token out of lexer
import CoALP.Parser.Lexer (Token(..))

-- TODO refactor program data types out of parser
import CoALP.Program (Program, Clause(..), Term(..))


-- | Lexer output pretty printer
ppLexer :: [Token] -> String
ppLexer t = concat $ foldr f [] t
	where
		f :: Token -> [String] -> [String]
		f tok res
			| [] <- res		= [show tok]
			| otherwise		= (show tok):(", "):res


ppProgram :: (Show a, Show b, Show c) => Program a b c -> String
ppProgram = concat . intersperse "\n" . map ppClause 

ppClause :: (Show a, Show b, Show c) => Clause a b c -> String
ppClause (Clause h (bs@(_:_))) = ppTerm h ++ " :- " ++ ppTerms bs ++ "."
ppClause (Clause h (_)) = ppTerm h ++ "."

ppTerms :: (Show a, Show b, Show c) =>  [Term a b c] -> String
ppTerms ts = concat . intersperse ", " . map ppTerm $ ts

ppTerm :: (Show a, Show b, Show c) => Term a b c -> String
ppTerm (Var x) = "_v" ++ show x
ppTerm (Fun f ts) = show f ++ "(" ++ ppTerms ts ++ ")"
ppTerm (Const i) = show i



