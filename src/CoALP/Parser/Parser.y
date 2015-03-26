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
%monad { Alex } { >>= } { return }
%error { parseError }
%lexer { scanTokens } { TEof }

%token 
	funId           { TFunId $$ }
	varId           { TVarId $$ }
	int             { TInt $$ }
	':-'		{ THBSep }
	'.'		{ TClauseTer }
	','		{ TTermSep }
--	'='             { Sym '=' }
--	'+'             { Sym '+' }
--	'-'             { Sym '-' }
--	'*'             { Sym '*' }
--	'/'             { Sym '/' } 
	'('             { TLBrace }
	')'		{ TRBrace }

%%

Program :: { [ Clause ] }
Program : Clauses			{ $1 }

Clauses :: { [ Clause ] }
Clauses	: Clauses Clause		{ $2 : $1 }
     	| Clauses 			{ $1 }
	| {- empty -}			{ [ ] }

Clause :: { Clause }
Clause	: Term ':-' Terms '.'		{ Clause $1 $3 }
	| Term '.'			{ Clause $1 [] }


Terms :: { [ Term ] }
Terms	: Terms ',' Term		{ $3 : $1 }
	| Term				{ [$1] }
	  {- we do not allow empty body of the clause -}

Term :: { Term }
Term	: funId '(' Terms ')'		{ Fun $1 $3 }
	| funId				{ Fun $1 [] }
	| varId				{ Var $1 }

--lineno :: { LineNumber }
--	: {- empty -}      {% getLineNo }

{

--parseError :: Token -> Alex a
--parseError t = throwE $ ParserErr $ show t
--parseError t = alexError $ "syntactic error: " ++ show t --  fail $ "Happy error: " ++ show t

parseError :: Token -> Alex a
parseError t = alexSynError t

type LineNumber = Int

type Program = [Clause]

data Clause = Clause Term [Term] 
	deriving Show

data Term
	= Var String
	| Fun String [Term]
	| Const Int
	deriving Show


test :: String -> Either String Program
test s = runAlex s main



}
