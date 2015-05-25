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
	| GC1
	| DrawProgram
	| GC2 String
	| DrawRew Int String
	| DrawTrans Int Integer String
	| Help
	deriving Show


