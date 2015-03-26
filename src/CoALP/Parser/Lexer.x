{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module CoALP.Parser.Lexer {-(
	  scanTokens
	, Token (..)
	) -} where

import Control.Monad.Trans.Except (Except, throwE)

import CoALP.Error (Err(ParserErr))

import Debug.Trace

}

%wrapper "monad"

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

  -- read numeric constant
  $digit+			{ \a len -> return $ TInt (read $ tokenStr a len) }

  -- read variable name
  $upper [$alphanum \_ \' ]*	{ \a len -> return $ TVarId (tokenStr a len) }

  -- read function name
  [$lower $symbol] # [\)\(] [$alphanum \_ \' ]*	{ \a len -> return $ TFunId (tokenStr a len) }

  -- read symbol
  -- $symbol			{ \a len -> return $ TSym (head $ tokenStr a len) }

  -- opening brace
  "("				{ \_ _ -> return TLBrace }

  -- closing brace
  ")"				{ \_ _ -> return TRBrace }



{

-- Lexer actions

-- -----------------------------------------------------------------------------
-- | The token type:
data Token =
	TFunId String	|
	TInt Int        |
	TVarId String	|
--	TNl Int		|
	TLBrace		|
	TRBrace		|
--	Sym Char	|
--	TComment String	|
	THBSep		|
	TTermSep	|
	TClauseTer	|
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
		ppTok (TInt i) = "constant " ++ show i
		ppTok TLBrace = "opening (" 
		ppTok TRBrace = "closing )"
		ppTok t = "token " ++ show t

		len (TVarId s) = length s
		len (TFunId s) = length s
		len (TInt i) = length . show $ i
		len TLBrace = 1
		len TRBrace = 1
		len t = length . show $ t



}
