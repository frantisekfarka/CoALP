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
	| GC2 String
	| GC3
	| DrawProgram
	| DrawRew Int String
	| DrawTrans Int [Integer] String
	| DrawDer Int Int String
	| Help
	| Empty
	deriving Show


