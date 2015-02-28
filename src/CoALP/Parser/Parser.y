{
module CoALP.Parser.Parser 
where

import Control.Monad.Trans.Except (Except, throwE)

import CoALP.Error (Err(ParserErr))

--TODO refactor
import CoALP.Parser.Lexer

import Data.Char

}

%name calc
%tokentype { Token }
%monad { Alex }
%error { parseError }
%lexer { scanTokens } { Eof }

%token 
	let             { Let }
	in              { In }
	int             { Int $$ }
	var             { Var $$ }
	'='             { Sym '=' }
	'+'             { Sym '+' }
	'-'             { Sym '-' }
	'*'             { Sym '*' }
	'/'             { Sym '/' } 
	'('             { Sym '(' }
	')'		{ Sym ')' }
	nl		{ Newline $$ }

%%

Program :: { [ Exp ] }
Program : Exps 				{ $1 }

Exps :: { [ Exp ] }
Exps	: Exps nl Exp			{ $3 : $1 }
     	| Exps nl			{ $1 }
	| Exp				{ [ $1] }
	| {- empty -}			{ [ ] }

Exp :: { Exp }
Exp	: let var '=' Exp in Exp	{ ExpLet $2 $4 $6 }
	| Exp1				{ Exp1 $1 }

Exp1 :: { Exp1 }
Exp1	: Exp1 '+' Term			{ Plus $1 $3 }
	| Exp1 '-' Term			{ Minus $1 $3 }
	| Term				{ Term $1 }

Term :: { Term }
Term	: Term '*' Factor		{ Times $1 $3 }
	| Term '/' Factor		{ Div $1 $3 }
	| Factor			{ Factor $1 }

Factor :: { Factor }
Factor	: int				{ FInt $1 }
	| var				{ FVar $1 }
	| '(' Exp ')'			{ Brack $2 }

{

parseError :: Token -> Alex a
--parseError t = throwE $ ParserErr $ show t
parseError t = alexError $ "Parser error: " ++ show t --  fail $ "Happy error: " ++ show t


type Program = [Exp]

data Exp 
	= ExpLet String Exp Exp
	| Exp1 Exp1
	deriving Show

data Exp1
	= Plus Exp1 Term 
	| Minus Exp1 Term 
	| Term Term
	deriving Show

data Term
	= Times Term Factor 
	| Div Term Factor 
	| Factor Factor
	deriving Show

data Factor 
	= FInt Int 
	| FVar String 
	| Brack Exp
	deriving Show



test :: String -> Either String Program
test s = runAlex s calc



}
