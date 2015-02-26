-- | REPL Parser
-- parses interpret commands
--
module CoALPj.REPL.Parser (
	parseCmd
) where

import Control.Applicative 
import Text.Parsec (parse,(<?>))
import Text.Parsec.Char (char,space,spaces,anyChar,noneOf,string,endOfLine)
import Text.Parsec.Combinator (eof,manyTill)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)

import CoALPj.REPL.Commands (Command(..))

parseCmd :: String -> Either ParseError Command
-- TODO input name
parseCmd cmd = parse pCmd "(input)" cmd


-- | simple Parsec command line parser
pCmd :: Parser Command
pCmd = spaces *> (
	pCommand
	<|> pOther
	)
	-- | TODO ensure oef
	-- *> (manyTill eof)
	where
		pCommand = char ':' *> (
			pLoad
			<|> pReload
			)
		pLoad = Load <$> (
			string "l"
			*> optional (string "oad")
			*> space *> spaces
			*> many anyChar
			)
		pReload = const Reload <$> (
			string "r"
			*> optional (string "eload")
			*> spaces
			)
		pOther = Other <$> (
			many anyChar
			)

