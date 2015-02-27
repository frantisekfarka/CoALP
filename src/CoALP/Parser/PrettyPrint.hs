-- | CoALP parser pretty printing helpers
module CoALP.Parser.PrettyPrint 
where


-- TODO refactor token out of lexer
import CoALP.Parser.Lexer


-- | Lexer output pretty printer
ppLexer :: [Token] -> String
ppLexer t = concat $ foldr f [] t
	where
		f :: Token -> [String] -> [String]
		f tok res
			| [] <- res		= [show tok]
			| (Newline _) <- tok	= (show tok):("\n"):res
			| otherwise		= (show tok):(", "):res

