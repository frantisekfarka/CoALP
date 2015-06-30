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
	| GC3One String
	| DrawProgram
	| DrawRew Integer String
	| DrawTrans Integer [Integer] String
	| DrawDer Integer Integer String
	| DrawInf Integer Integer String
	| DrawUng Integer Integer String
	| Help
	| Empty
	deriving Show


