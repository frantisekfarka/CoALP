{
module CoALP.Parser.Parser 
where

import Control.Monad.Trans.Except (Except, throwE)

import CoALP.Error (Err(ParserErr))

--TODO refactor
import CoALP.Parser.Lexer
import CoALP.Program

import Data.Char
import Data.Map(empty,findWithDefault,insert)

}

%name main Clauses
%name query Query
%tokentype { Token }
%monad { Alex } { >>= } { return }
%error { parseError }
%lexer { scanTokens } { TEof }

%token 
	funId           { TFunId $$ }
	varId           { TVarId $$ }
--	int             { TInt $$ }
	':-'		{ THBSep }
	'.'		{ TClauseTer }
	','		{ TTermSep }
	'('             { TLPar }
	')'		{ TRPar }
	'?'		{ TQuery }

%%

Clauses :: { Program1 }
-- ^ we do not clear vars as matching currently does not assign fresh vars
Clauses	: Clauses Clause		{% {- clearVars >> -} return ($2 : $1) }
	| {- empty -}			{ [ ] }

Clause :: { Clause1 }
Clause	: Term ':-' Terms '.'		{ Clause $1 (reverse $3) }
	| Term '.'			{ Clause $1 [] }

Query :: { Query1 }
Query	: '?' ':-' Terms '.'		{ Query $3 }

Terms :: { [ Term1 ] }
Terms	: Terms ',' Term		{ $3 : $1 }
	| Term				{ [$1] }
	  {- we do not allow empty body of the clause -}

Term :: { Term1 }
Term	: funId '(' Terms ')'		{ Fun $1 (reverse $3) }
	| funId				{ Fun $1 [] } 
	| varId				{% getVar $1 >>= return . Var }
--	| int				{ Const $1 }

{

--parseError :: Token -> Alex a
--parseError t = throwE $ ParserErr $ show t
--parseError t = alexError $ "syntactic error: " ++ show t --  fail $ "Happy error: " ++ show t

parseError :: Token -> Alex a
parseError t = alexSynError t


parse :: String -> Either String Program1
parse s = runAlex s main


parseQuery :: String -> Either String Query1
parseQuery s = runAlex s query



}
