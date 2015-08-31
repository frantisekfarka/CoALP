{
-- The Happy parser
module CoALP.Parser.Parser (
	  parse
	, parseWithCount
	, parseClause
) where

import Control.Arrow ((***))
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
	, alexError
	, alexSynError
	, alexGetPos
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

--Program :: { (Program1, Signature1) }
--Program : Clauses {% getSig >>= \s -> return ($1, s) }
Program : Program Clause 		{ (($2 : ) *** id) $1 }
	| Program SigSpec		{% let ((c, s),(iden, t)) = ($1, $2)
					   in case (markType s iden t) of
					   	Left t'   -> repSig iden t'
						Right s' -> return (c, s') }
	| {- empty -}			{ ([], empty) }

--Clauses :: { [Clause1] } 
-- ^ we do not clear vars as matching currently does not assign fresh vars
--Clauses	: Clauses Clause		{% clearVars >> return ($2 : $1) }
--	| Clauses IndSpec		{% alexError "ada"  }
--	| {- empty -}			{ ([ ] }

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


SigSpec :: { (Ident, Type) }
SigSpec : SigT ':' funId		{ ($3, $1) } 

SigT :: { Type }
SigT : 'inductive'	{ SInd }
     | 'coinductive'	{ SCoInd }


{

--repSig :: Ident -> Type -> Alex Ident
repSig iden t = do
	(l, c) <- alexGetPos
	alexError ("Duplicate type signature at line " ++ show l ++
		": '" ++ iden ++ "' already marked ")

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
