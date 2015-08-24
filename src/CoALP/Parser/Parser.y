{
-- The Happy parser
module CoALP.Parser.Parser (
	  parse
	, parseWithCount
	, parseClause
) where

import Control.Monad.Trans.Except (Except, throwE)

import CoALP.Error (Err(ParserErr))

--TODO refactor
import CoALP.Parser.Lexer (
	  Alex
	, Token (..)
	, runAlex
	, runAlex'
	, getVar
	, scanTokens
	, clearVars
	, alexSynError
	, getSig
	)

import CoALP.Program

import Data.Char
import Data.Map(empty,findWithDefault,insert)

}

%name main Program
%name clause Clause
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
	'('             { TLPar }
	')'		{ TRPar }
--	'?'		{ TQuery }
	':'		{ TTSep }
	'inductive'	{ TInd }
	'coinductive'	{ TCoInd }

%%

Program :: { (Program1, Signature1) }
Program : Clauses {% getSig >>= \s -> return ($1, s) }

Clauses :: { [Clause1] } 
-- ^ we do not clear vars as matching currently does not assign fresh vars
Clauses	: Clauses Clause		{% clearVars >> return ($2 : $1) }
	| Clauses IndSpec		{ $1 }
	| {- empty -}			{ [ ] }

Clause :: { Clause1 }
Clause	: Term ':-' Terms '.'		{ Clause $1 (reverse $3) }
	| Term '.'			{ Clause $1 [] }

-- Query :: { Query1 }
-- Query	: '?' ':-' Terms '.'		{ Query $3 }

Terms :: { [ Term1 ] }
Terms	: Terms ',' Term		{ $3 : $1 }
	| Term				{ [$1] }
	  {- we do not allow empty body of the clause -}

Term :: { Term1 }
Term	: funId '(' Terms ')'		{ Fun $1 (reverse $3) }
	| funId				{ Fun $1 [] } 
	| varId				{% getVar $1 >>= return . Var }
	| int				{ Fun (show $1) [] } -- little hack for now
--	| int				{ Const $1 }


IndSpec : 'inductive' ':' funId		{ () }


{

-- | Handle syntactic error
parseError :: Token -> Alex a
parseError t = alexSynError t


-- | Parse program
parse :: String -> Either String (Program1, Signature1)
parse s = runAlex s main

-- | Parse program, obtain next variable counter 
parseWithCount :: String -> Either String ((Program1, Signature1), Integer)
parseWithCount s = runAlex' s main

-- | Parse single clause
parseClause :: String -> Either String Clause1 
parseClause s = runAlex s clause 



}
