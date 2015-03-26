-- | CoALP parser pretty printing helpers
module CoALP.Parser.PrettyPrint 
where

import Data.List (intersperse)


-- TODO refactor token out of lexer
import CoALP.Parser.Lexer (Token(..))

-- TODO refactor program data types out of parser
import CoALP.Parser.Parser (Program, Clause(..), Term(..))


-- | Lexer output pretty printer
ppLexer :: [Token] -> String
ppLexer t = concat $ foldr f [] t
	where
		f :: Token -> [String] -> [String]
		f tok res
			| [] <- res		= [show tok]
			| otherwise		= (show tok):(", "):res


ppProgram :: Program -> String
ppProgram = concat . intersperse "\n" . map ppClause 

ppClause :: Clause -> String
ppClause (Clause h (bs@(_:_))) = ppTerm h ++ " :- " ++ ppTerms bs ++ "."
ppClause (Clause h (_)) = ppTerm h ++ "."

ppTerms :: [Term] -> String
ppTerms ts = concat . intersperse ", " . map ppTerm $ ts

ppTerm :: Term -> String
ppTerm (Var x) = x
ppTerm (Fun f ts) = f ++ "(" ++ ppTerms ts ++ ")"
ppTerm (Const i) = show i



