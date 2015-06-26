{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module CoALP.Parser.Lexer {-(
	  scanTokens
	, Token (..)
	) -} where

--import Control.Monad.Trans.Except (Except, throwE)
import Data.Map as M (Map,empty,insert, lookup) 

--import CoALP.Error (Err(ParserErr))
import CoALP.Program (
	  Ident
	-- , Constant
	, Variable
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
--  $digit+			{ \a len -> return $ TInt (read $ tokenStr a len) }

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



{

-- -----------------------------------------------------------------------------
-- | The User State type:
data AlexUserState = AlexUserState {
	  counter :: Variable
	, vars :: Map Ident Variable
	}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
	  counter = (1)
	, vars = empty
	}

getVar :: Ident -> Alex Variable
getVar ident = Alex $ \s@AlexState{alex_ust=ust} 
	-> case (ident `M.lookup` (vars ust)) of
		Just i  -> Right (s, i)
		Nothing -> Right (s{alex_ust=(alex_ust s){
			  counter=(counter ust + 1)
			, vars=(insert ident (counter ust + 1) (vars ust))
			}}, counter ust + 1)
			
clearVars :: Alex ()
clearVars = Alex $ \s@AlexState{alex_ust=ust}
	-> Right (s{alex_ust=ust{vars=empty}}, ())



-- -----------------------------------------------------------------------------
-- | The token type:
data Token =
	TFunId Ident    |
--	TInt Constant   | -- lets keep it out for now
	TVarId Ident    |
	TLPar           |
	TRPar           |
	THBSep		|
	TTermSep	|
	TClauseTer	|
	TQuery		|
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

scanTokens :: (Token -> Alex a) -> Alex a
scanTokens = (alexMonadScan >>=)

alexEOF :: Alex Token
alexEOF = return TEof

-- | Sybtactioc error (unexpected token)
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
--		ppTok (TInt i) = "constant " ++ show i
		ppTok TQuery = "token '?'" 
		ppTok TLPar = "opening (" 
		ppTok TRPar = "closing )"
		ppTok t = "token " ++ show t

		len (TVarId s) = length s
		len (TFunId s) = length s
--		len (TInt i) = length . show $ i
		len TQuery = 1
		len TLPar = 1
		len TRPar = 1
		len t = length . show $ t



}
