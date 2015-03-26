{
module CoALP.Parser.Parser 
where

import Control.Monad.Trans.Except (Except, throwE)

import CoALP.Error (Err(ParserErr))

--TODO refactor
import CoALP.Parser.Lexer

import Data.Char

}

%name main
%tokentype { Token }
%monad { Alex }
%error { parseError }
%lexer { scanTokens } { Eof }

%token 
	fun             { Fun $$ }
	var             { Var $$ }
	int             { Int $$ }
	':-'		{ Sep }
	'.'		{ ClauseTer }
	','		{ TermSep }
--	'='             { Sym '=' }
--	'+'             { Sym '+' }
--	'-'             { Sym '-' }
--	'*'             { Sym '*' }
--	'/'             { Sym '/' } 
	'('             { Sym '(' }
	')'		{ Sym ')' }
	nl		{ Newline $$ }

%%

Program :: { [ Clause ] }
Program : Clauses			{ $1 }

Clauses :: { [ Clause ] }
Clauses	: Clauses nl Clause		{ $3 : $1 }
     	| Clauses nl			{ $1 }
	| Clause			{ [ $1] }
	| {- empty -}			{ [ ] }

Clause :: { Clause }
Clause	: Term ':-' Terms '.'		{ Clause $1 $3 }
	| Term '.'			{ Clause $1 [] }


Terms :: { [ Term ] }
Terms	: Terms ',' Term		{ $3 : $1 }
	| Term				{ [$1] }
	  {- we do not allow empty body of the clause -}

Term :: { Term }
Term	: fun '(' Terms ')'		{ TFun $1 $3 }
	| fun				{ TFun $1 [] }
	| var				{ TVar $1 }

{

parseError :: Token -> Alex a
--parseError t = throwE $ ParserErr $ show t
parseError t = alexError $ "Parser error: " ++ show t --  fail $ "Happy error: " ++ show t


type Program = [Clause]

data Clause = Clause Term [Term] 
	deriving Show

data Term
	= TVar String
	| TFun String [Term]
	| TConst Int
	deriving Show


test :: String -> Either String Program
test s = runAlex s main



}
