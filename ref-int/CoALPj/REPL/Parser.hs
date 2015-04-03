-- | REPL Parser
-- parses interpret commands
--
module CoALPj.REPL.Parser (
	  parseCmd
	, replCompletion
) where

import Control.Applicative 
import Control.Monad.IO.Class
import Text.Parsec (parse,(<?>),many1)
import Text.Parsec.Char (char,space,spaces,anyChar,noneOf,string,endOfLine)
import Text.Parsec.Combinator (eof,manyTill)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)

import System.Console.Haskeline.Completion -- (Completion(..), CompletionFunc, noCompletion)

import CoALPj.REPL.Commands (Command(..))

parseCmd :: String -> Either ParseError Command
-- TODO input name
parseCmd cmd = parse pCmd "(input)" cmd


-- | simple Parsec command line parser
pCmd :: Parser Command
pCmd = spaces 
	*> pCommand
	<* spaces
	<* eof
	where
		spaces1 = many1 space
		pCommand = char ':' 
			*> (
				pLoad
				<|> pReload
				<|> pPrintPray
			)
		pLoad = string "l"
			*> optional (string "oad")
			*> spaces1
			*> (
				Load 
				<$> many anyChar
			)
		pReload = string "r"
			*> optional (string "eload")
			*> pure Reload 
		pPrintPray = string "pr" *> 
			(
			pPrint
			<|> pPray
			)
		pPrint = string "i"
			*> optional (string "nt")
			*> pure Print 
		pPray = string "a"
			*> optional (string "y")
			*> spaces1
			*> (
				Pray
				<$> (
					char '"'
					*> anyChar `manyTill` (char '"')
				)
			)


data CommandDescr = Sep String [CommandDescr] | Cmd String String
	deriving Show	

myCmds = Sep ":" [
	-- | Load
	  Sep "l" [
	  	Cmd "oad" "lets load"
	]
	-- | Reload
	, Sep "r" [
		Cmd "eload" "we reload!"
	]
	-- | Print or Pray
	, Sep "pr" [
		-- | Pr{a}y
		  Sep "a" [
		  	Cmd "y" "Should pray"
		]
		-- | Pr{i}nt
		, Sep "i" [
			Cmd "nt" "Could print"
		]
	]
	]


-- | Complete REPL commands and defined identifiers
-- TODO proper implemnetation
replCompletion :: MonadIO m => CompletionFunc m
{-replCompletion (prev, next) = return ( "", fmap compl [
		  " -- TODO implement completion"
		, " -- you can always try other comletions ..."
		])
	where
		compl x = Completion {
			  replacement = x
			, display = "try " ++ x
			, isFinished = False
			}
-}
replCompletion = completeFilename
--replCompletion = completeWord (Nothing) " " replcs
	where
		replcs s = do
			return [
				  simpleCompletion "none"
				, simpleCompletion "nil"
				]


