-- | REPL command definition and completion helper
module CoALPj.REPL.Commands (
	 Command(..)
) where

-- | Interpreter commands
data Command
	= Load FilePath
	| Reload
	| Quit
	| Other String
	deriving Show


