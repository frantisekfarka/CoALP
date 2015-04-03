-- | REPL command definition and completion helper
module CoALPj.REPL.Commands (
	 Command(..)
) where

-- | Interpreter commands
data Command
	= Load FilePath
	| Reload
	| Print
	| Quit
	| Pray String
	deriving Show

{-
data CommandDescr = ...

commandDescr :: CommandDescr 
commandDescr = loadCmd filePath
	<|> reloadCmd
	<|> printCmd
	<|> prayCmd string


parserCmd :: CommandDescr -> Parser Command
	--T--> transforms to parser

completionCmd :: Monad m => CommandDescr CompletionFunc m
	--T--> transfroms to completion func
-}

