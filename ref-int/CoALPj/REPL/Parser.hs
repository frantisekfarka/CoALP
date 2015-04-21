{-# LANGUAGE FlexibleInstances #-}

-- | REPL Parser
-- parses interpret commands
--
module CoALPj.REPL.Parser (
	  parseCmd
	, replCompletion
) where

import Control.Applicative 
import Control.Monad.IO.Class
import Control.Arrow ((***))
import Data.Monoid (Monoid,mempty,mappend,mconcat)
import Text.Parsec (parse,many1,digit,eof)
import Text.Parsec.Char (char,space,spaces,anyChar,noneOf,string)
--import Text.Parsec.Combinator (eof,manyTill)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)

import System.Console.Haskeline.Completion -- (Completion(..), CompletionFunc, noCompletion)

import CoALPj.REPL.Commands (Command(..))


parseCmd :: String -> Either ParseError Command
-- TODO input name
parseCmd cmd = parse (toCmdParser dCmd <* spaces <* eof) "(input)" cmd

--dCmd :: Parser Command 
dCmd :: CommandDescr String String (Parser Command)
dCmd = toCmdDescr [
	  (":load",	spaces *> (Load <$> many (noneOf " \t\\" <|> (char '\\' *> anyChar))))
	, (":reload",	pure Reload)
	, (":print",	pure Print)
	, (":quit",	pure Quit)
	, (":gc1",	pure GC1)
	, (":gc2",	spaces *> (GC2 <$> many anyChar))
	, (":drawTerms",pure DrawProgram)
	, (":drawRew",	spaces *> (DrawRew <$> (read <$> digits1 <* spaces1) <*> many anyChar))
	, (":drawTrans",spaces *> (DrawTrans
		<$> (read <$> digits1 <* spaces1) 
		<*> (read <$> digits1 <* spaces1) 
		<*> many anyChar))
	]

-- | Just a helpers
spaces1,digits1 :: Parser String
spaces1 = many1 space
digits1 = many1 digit


{-
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
		pCommand = char ':' 
			*> (
				pLoad
				<|> pReload
				<|> pPrint
				<|> pGC
				<|> pQuit
				<|> pDraw
			)
		pLoad = string "l"
			*> optional (string "oad")
			*> spaces1
			*> (
				Load <$> many (noneOf " \t\\" <|> (char '\\' *> anyChar))
			)
		pReload = string "r"
			*> optional (string "eload")
			*> pure Reload 
		pPrint = string "p"
			*> optional (string "rint")
			*> pure Print 
		pQuit = string "q"
			*> optional (string "uit")
			*> pure Quit
		pGC = string "gc"
			*> (
				(string "1" *> pure GC1)
				<|> (string "2" *> pure GC2)
			)
		pDraw = string "draw" 
			*> (
				(
					string "T" *> optional (string "erm") *> pure DrawProgram
				) <|> (
					string "R" *> optional (string "ew") *> 
					spaces *> (DrawRew <$> (read <$> digits1 <* spaces1) <*> many anyChar
					)
				)
			)
-}

-- | Command Decription Trie
-- use Data - trie
data CommandDescr a b c = Sep [(a,CommandDescr a b c)] | Cmd b c
	deriving Show	

instance Ord a => Monoid (CommandDescr a [a] c) where
	mempty = emptyCmds
	mappend = joinCmds

emptyCmds :: CommandDescr a b c
emptyCmds = Sep []

joinCmds :: (Ord a) => CommandDescr a [a] c -> CommandDescr a [a] c -> CommandDescr a [a] c
joinCmds (Sep [])		y		= y
joinCmds x			(Sep [])	= x
--joinCmds (Cmd [] _)		(Cmd [] _) 	= error "multiple commands with same string"
joinCmds (Cmd (x1:xs1) y1)	(Cmd (x2:xs2) y2) 
	| x1 == x2	= Sep [(x1,joinCmds (Cmd xs1 y1) (Cmd xs2 y2))]
	| x1 < x2	= Sep [(x1,Cmd xs1 y1),(x2,Cmd xs2 y2)]
	| otherwise 	= Sep [(x2,Cmd xs2 y2),(x1,Cmd xs1 y1)]
joinCmds (Sep s1) (Sep s2) = Sep $ joinS (s1) s2
	where
		joinS [] x = x
		joinS x [] = x
		joinS (a@((x,xc):xcs)) (b@((y,yc):ycs)) 
			| x < y		= (x,xc):(joinS xcs b)
			| x == y 	= (x, joinCmds xc yc):(joinS xcs ycs)
			| x > y		= (y,yc):(joinS a ycs)
		joinS _ _ = error "Impssible pattern2?"

joinCmds a@(Sep _) (Cmd (x:xs) xc)	= joinCmds a (Sep [(x,Cmd xs xc)])
joinCmds a@(Cmd (_:_) _) b@(Sep _)	= joinCmds b a
joinCmds _ _ = error "Impossible pattern?" -- $ show x ++ "\n" ++ show y

-- | pack the trie
pack :: CommandDescr a [a] c -> CommandDescr [a] [a] c
pack (Cmd x c)		= Cmd x c
pack (Sep [(x,Cmd xs c)])	= Cmd (x:xs) c
pack (Sep s)		= Sep $ ((:[]) *** pack) <$> s

-- | concat partial parsers
toCmdDescr :: Ord a => [([a],c)] -> CommandDescr [a] [a] c
toCmdDescr p = pack $ mconcat $ (uncurry Cmd) <$> p

toCmdParser :: CommandDescr String String (Parser a) -> Parser a
toCmdParser (Cmd c cmd) = optional (string c) *> cmd
toCmdParser (Sep s) 	= foldr (<|>) empty (step <$> s)
	where
		step (sep, c) = string sep *> toCmdParser c

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
{-		replcs _ = do
			return [
				  simpleCompletion "none"
				, simpleCompletion "nil"
				]
-}

