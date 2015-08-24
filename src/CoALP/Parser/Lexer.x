{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | Alex the lexer
module CoALP.Parser.Lexer (
	  Alex
	, Token (..)
	, runAlex
	, runAlex'
	, getVar
	, scanTokens
	, clearVars
	, alexSynError
	, getSig
	) where

import Data.Map.Strict as M (Map,empty,insert, lookup) 

--import CoALP.Error (Err(ParserErr))
import CoALP.Program (
	  Ident
	, Constant
	, Var
	, Type
	)


}

%wrapper "monadUserState"

$digit 		= [0-9]			-- digits
$alpha 		= [a-zA-Z]		-- alphabetic characters
$alphanum	= [$alpha $digit]	-- alphabetic characters
$lower		= [a-z]			-- lowercase characters
$upper		= [A-Z]			-- uppercase characters
$symbol		= [\=\+\-\*\/]	-- symbol

$whitenonl = [\ \t\f\v]		-- whitespace no newline
$newline = [\n\r]		-- newline

tokens :-

  -- ignore all whitespace
  $white+ 			;
  --$whitenonl+ 			;

  -- we need to count newlines (for pretty printing of input, error msgs)
  -- $newline+			{ \a _ -> return (TNl (line a))  }

  -- inductive keyword
  "inductive"			{ \_ _ -> return TInd }

  -- coinductive keyword
  "coinductive"			{ \_ _ -> return TCoInd }

  -- we skip comments
  "%" .*			; --{ \a len -> return $ TComment $ tokenStr a len}

  -- clause head - body separator
  ":-"				{ \_ _ -> return THBSep }

  -- term separator
  ","				{ \_ _ -> return TTermSep }

  -- clause terminator
  "."				{ \_ _ -> return TClauseTer }

  -- query clause head
  "?"				{ \_ _ -> return TQuery }

  -- read numeric constant
  $digit+			{ \a len -> return $ TInt (read $ tokenStr a len) }

  -- read variable name
  $upper [$alphanum \_ \' ]*	{ \a len -> return $ TVarId (tokenStr a len) }

  -- read function name
  [$lower $symbol] # [\)\(] [$alphanum \_ \' ]*	{ \a len -> return $ TFunId (tokenStr a len) }

  -- read symbol
  -- $symbol			{ \a len -> return $ TSym (head $ tokenStr a len) }

  -- opening brace
  "("				{ \_ _ -> return TLPar }

  -- closing brace
  ")"				{ \_ _ -> return TRPar }

  -- type separator
  ":"				{ \_ _ -> return TTSep }




{

-- -----------------------------------------------------------------------------
-- | The User State type:
data AlexUserState = AlexUserState {
	  counter :: Var
	, vars :: Map Ident Var
	, sig :: Map Ident Type
	}

-- | Initialize user state
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
	  counter = (1)
	, vars = empty
	}

-- | Get var counter from user state
getVar :: Ident -> Alex Var
getVar ident = Alex $ \s@AlexState{alex_ust=ust} 
	-> case (ident `M.lookup` (vars ust)) of
		Just i  -> Right (s, i)
		Nothing -> Right (s{alex_ust=(alex_ust s){
			  counter=(counter ust + 1)
			, vars=(insert ident (counter ust + 1) (vars ust))
			}}, counter ust + 1)

-- | Get signature descriptor
getSig :: Alex (Map Ident Type)
getSig = Alex $ \s@AlexState{alex_ust=ust}
	-> Right (s, sig ust)

-- | Reset var counter
clearVars :: Alex ()
clearVars = Alex $ \s@AlexState{alex_ust=ust}
	-> Right (s{alex_ust=ust{vars=empty}}, ())

-- -----------------------------------------------------------------------------
-- | The token type:
data Token =
	TFunId Ident    |
	TInt Constant   | 
	TVarId Ident    |
	TLPar           |
	TRPar           |
	THBSep		|
	TTermSep	|
	TClauseTer	|
	TQuery		|
	TInd		|
	TCoInd		|
	TTSep		|
	TEof		
	deriving (Eq,Show)



-- | Position extraction helper
alexGetPos :: Alex (Int, Int)
alexGetPos = do
	((AlexPn _ l c), _, _, _) <- alexGetInput
	return (l, c)

-- | Token extraction helper
tokenStr :: AlexInput -> Int -> String
tokenStr (_, _, _, s) len = take len s

-- | Bind alex monad
scanTokens :: (Token -> Alex a) -> Alex a
scanTokens = (alexMonadScan >>=)

-- | EOF token
alexEOF :: Alex Token
alexEOF = return TEof

-- | Syntactic error (unexpected token)
alexSynError :: Token -> Alex a
alexSynError tok = do
	(l, c) <- alexGetPos
	alexError $
		"syntactic error at line " ++ show l 
		++ ", column " ++ show (c - (len tok))
		++ ": unexpected " ++ ppTok tok
	where
		ppTok (TVarId s) = "variable " ++ s
		ppTok (TFunId s) = "function identifier '" ++ s ++ "'"
		ppTok (TInt i) = "constant " ++ show i
		ppTok TQuery = "token '?'" 
		ppTok TLPar = "opening (" 
		ppTok TRPar = "closing )"
		ppTok t = "token " ++ show t

		len (TVarId s) = length s
		len (TFunId s) = length s
		len (TInt i) = length . show $ i
		len TQuery = 1
		len TLPar = 1
		len TRPar = 1
		len t = length . show $ t

-- | Run alex monad and get user state 
runAlex' :: String -> Alex a -> Either String (a, Integer)
runAlex' input (Alex f) = case f (AlexState {alex_pos = alexStartPos,
		alex_inp = input,       
		alex_chr = '\n',
		alex_bytes = [],
		alex_ust = alexInitUserState,
		alex_scd = 0}) of
			Left msg -> Left msg
			Right ( s, a ) -> Right (a, counter . alex_ust $ s)

}
