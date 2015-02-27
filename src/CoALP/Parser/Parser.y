{
module CoALP.Parser.Parser 
where

import CoALP.Parser.Lexer
import Data.Char

}

%name calc
%tokentype { Token }
%error { parseError }

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

Program : Exps 				{ $1 }

Exps	: Exps nl Exp			{ $3 : $1 }
     	| Exps nl			{ $1 }
	| Exp				{ [ $1] }
	| {- empty -}			{ [ ] }

Exp	: let var '=' Exp in Exp	{ ExpLet $2 $4 $6 }
	| Exp1				{ Exp1 $1 }

Exp1	: Exp1 '+' Term			{ Plus $1 $3 }
	| Exp1 '-' Term			{ Minus $1 $3 }
	| Term				{ Term $1 }

Term	: Term '*' Factor		{ Times $1 $3 }
	| Term '/' Factor		{ Div $1 $3 }
	| Factor			{ Factor $1 }

Factor	: int				{ FInt $1 }
	| var				{ FVar $1 }
	| '(' Exp ')'			{ Brack $2 }

{

parseError :: [Token] -> a
parseError t = error $ "Parse error at " ++ show t


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


main = getContents >>= print . calc . scanTokens

test = print . calc . scanTokens

}
