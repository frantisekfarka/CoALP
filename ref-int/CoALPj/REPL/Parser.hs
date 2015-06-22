{-# LANGUAGE FlexibleInstances #-}

-- | REPL Parser
-- parses interpret commands
--
module CoALPj.REPL.Parser (
	  parseCmd
	, cmdInfo
	, replCompletion
) where

import Control.Applicative 
import Control.Monad.IO.Class (MonadIO)
import Control.Arrow ((***))
import Data.Maybe (fromJust)
import Data.Monoid (Monoid,mempty,mappend,mconcat)
import Text.Parsec (parse,many1,digit,eof)
import Text.Parsec.Char (char,space,spaces,anyChar,noneOf,string,hexDigit)
import Text.Parsec.Combinator (sepBy)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)

import System.Console.Haskeline.Completion -- (Completion(..), CompletionFunc, noCompletion)

import CoALPj.REPL.Commands (Command(..))


parseCmd :: String -> Either ParseError Command
-- TODO input name
parseCmd cmd = parse (
	(
		(spaces *> pure Empty <* eof)
		<|> toCmdParser dCmd
	) <* spaces <* eof) "(input)" cmd

cmdInfo :: String
cmdInfo = toCmdInfo dCmd

--dCmd :: Parser Command 
dCmd :: CommandDescr String String (Parser Command)
dCmd = toCmdDescr [
	  (
	  	":load"
		, spaces *> (Load <$> many (noneOf " \t\\" <|> (char '\\' *> anyChar)))
	  	, "\n\t:load <file>\n\t\tLoad a new program\n"
	), (
		":reload"
		, pure Reload
		, "\n\t:reload\n\t\tReload the current program\n"
	), (
		":print"
		, pure Print
		, "\n\t:print\n\t\tPrint contents of the loaded program\n"
	), (
		":quit"
		, pure Quit
		, "" -- \n\t:quit\n\t\tExit the interpreter\n"
	), (
		":gc1"
		, pure GC1
		, "\n\t:gc1\n\t\tGuardednes check 1\n"
	), (
		":gc2"
		, spaces *> (GC2 <$> many anyChar)
		, "\n\t:gc2 <query>\n\t\tGuardedness check 2, query bas the form\n" ++
		"\t\t'? :- BODY . '\n"
	), (
		":gc3"
		, spaces *> (GC3 <$> many anyChar)
		, "\n\t:gc3 <query>\n\t\tGuardedness check 3, for a clause for now, query bas the form\n" ++
		"\t\t'? :- BODY . '\n"
	), (
		":drawTerms"
		, pure DrawProgram
		, "\n\t:drawTerms\n\t\tDraw terms in the program\n"
	), (
		":drawRew"
		, spaces *> (DrawRew <$> (read <$> digits1 <* spaces1) <*> many anyChar)
		, "\n\t:drawRew <depth> <query>\n\t\tDraw rewriting tree, depth is an integer, query has the form\n" ++
		"\t\t'? :- BODY . '\n"
	), (
		":drawTrans"
		, spaces *> (DrawTrans
		<$> (read <$> digits1 <* spaces1) 
		<*> (fmap readHex <$>
		(
			string "[" *> 
			((spaces *> string "0x" *> hexDigits1 <* spaces) `sepBy` (string ","))
			<* string "]" <* spaces1
		)) <*> many anyChar)
		, "\n\t:drawTrans <depth> '[' <transvar_1>, ... ']' <query>\n\t\tDraw transition between rewriting trees, depth is an integer,\n" ++
		"\t\ttransvars are the transition variable, and query has the form\n" ++
		"\t\t'? :- BODY . '\n"
	), (
		":drawDer"
		, spaces *> (DrawDer
		<$> (read <$> digits1 <* spaces1) 
		<*> (read <$> digits1 <* spaces1) 
		<*> many anyChar)
		, "\n\t:drawDer <depthDer> <depthRew> <query>\n\t\tDraw derivation tree, depth is an integer, and query has the form\n" ++
		"\t\t'? :- BODY . '\n"
	), (
		":help"
		, pure Help
		, "\n\t:help\n\t\tShow the help\n"
	)
	]

-- | Just a helpers
spaces1,digits1,hexDigits1 :: Parser String
spaces1 = many1 space
digits1 = many1 digit
hexDigits1 = many1 hexDigit

readHex :: (Integral a, Read a) => String -> a
readHex s = foldl (\x y -> 16 * x + toH y) 0 s
	where
		toH x = fromJust $ lookup x 
			[ ('0',  0), ('1',  1), ('2',  2), ('3',  3), ('4',  4)
			, ('5',  5), ('6',  6), ('7',  7), ('8',  8), ('9',  9)
			, ('a', 10), ('b', 11), ('c', 12), ('d', 13), ('e', 14), ('f', 15)
			, ('A', 10), ('B', 11), ('C', 12), ('D', 13), ('E', 14), ('F', 15)
			]

-- | Command Decription Trie
-- use Data - trie
data CommandDescr a b c = Sep [(a,CommandDescr a b c)] | Cmd b c String
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
joinCmds (Cmd (x1:xs1) y1 desc1)	(Cmd (x2:xs2) y2 desc2) 
	| x1 == x2	= Sep [(x1,joinCmds (Cmd xs1 y1 desc1) (Cmd xs2 y2 desc2))]
	| x1 < x2	= Sep [(x1,Cmd xs1 y1 desc1),(x2,Cmd xs2 y2 desc2)]
	| otherwise 	= Sep [(x2,Cmd xs2 y2 desc1),(x1,Cmd xs1 y1 desc2)]
joinCmds (Sep s1) (Sep s2) = Sep $ joinS (s1) s2
	where
		joinS [] x = x
		joinS x [] = x
		joinS (a@((x,xc):xcs)) (b@((y,yc):ycs)) 
			| x < y		= (x,xc):(joinS xcs b)
			| x == y 	= (x, joinCmds xc yc):(joinS xcs ycs)
			| x > y		= (y,yc):(joinS a ycs)
		joinS _ _ = error "Impssible pattern2?"

joinCmds a@(Sep _) (Cmd (x:xs) xc d)	= joinCmds a (Sep [(x,Cmd xs xc d)])
joinCmds a@(Cmd (_:_) _ _) b@(Sep _)	= joinCmds b a
joinCmds _ _ = error "Impossible pattern?" -- $ show x ++ "\n" ++ show y

-- | pack the trie
pack :: CommandDescr a [a] c -> CommandDescr [a] [a] c
pack (Cmd x c d)		= Cmd x c d
pack (Sep [(x,Cmd xs c d)])	= Cmd (x:xs) c d
pack (Sep s)		= Sep $ ((:[]) *** pack) <$> s

-- | concat partial parsers
toCmdDescr :: Ord a => [([a],c,String)] -> CommandDescr [a] [a] c
toCmdDescr p = pack $ mconcat $ u <$> p
	where
		u (a, b, c) = Cmd a b c 

toCmdParser :: CommandDescr String String (Parser a) -> Parser a
toCmdParser (Cmd c cmd _) = optional (string c) *> cmd
toCmdParser (Sep s) 	= foldr (<|>) empty (step <$> s)
	where
		step (sep, c) = string sep *> toCmdParser c

toCmdInfo :: CommandDescr a b c -> String
toCmdInfo (Cmd _ _ d)	= d
toCmdInfo (Sep s)	= foldr mappend empty (map (toCmdInfo . snd) s)

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

