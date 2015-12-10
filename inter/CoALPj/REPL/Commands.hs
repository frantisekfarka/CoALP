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
  | Transform
  | Annotate
  | Convert
  | AntiUnify String
	| GC1
	| GC2 String
	| GC3
	| GC3One String
	| DrawProgram
	| DrawRew Int String
	| DrawTrans Int [Integer] String
	| DrawDer Int Int String
	| DrawInf Int Int String
	| DrawUng Int Int String
	| DrawUnsafe Int Int String
	| Help
	| Empty
	| Resolve String
	| Next
	| Sig String
	deriving Show
