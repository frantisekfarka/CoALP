{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module CoALP.Parser.Lexer {-(
	  scanTokens
	, Token (..)
	) -} where

import Control.Monad.Trans.Except (Except, throwE)

import CoALP.Error (Err(ParserErr))

}

%wrapper "monad"

$digit 		= [0-9]			-- digits
$alpha 		= [a-zA-Z]		-- alphabetic characters
$alphanum	= [$alpha $digit]	-- alphabetic characters
$lower		= [a-z]			-- lowercase characters
$upper		= [A-Z]			-- uppercase characters
$symbol		= [\=\+\-\*\/\(\)]	-- symbol

$whitenonl = [\ \t\f\v]		-- whitespace no newline
$newline = [\n\r]		-- newline

tokens :-

  -- ignore all whitespace
  $whitenonl+ 			;

  -- we need to count newlines (TODO for pretty printing of input, error msgs?)
  $newline+			{ \a _ -> return (Newline (line a))  }

  -- we want to be able to pretty print comments in output
  "%" .*			{ \a len -> return $ Comment $ tokenStr a len
				}

  -- clause head - body separator
  ":-"				{ \_ _ -> return Sep }

  -- term separator
  ","				{ \_ _ -> return TermSep }

  -- clause terminator
  "."				{ \_ _ -> return ClauseTer }

  -- read numeric constant
  $digit+			{ \a len -> return $ Int (read $ tokenStr a len) }

  -- read variable name
  $upper [$alphanum \_ \' ]*	{ \a len -> return $ Var (tokenStr a len) }

  -- read function name
  [$lower $symbol] # [\)\(] [$alphanum \_ \' ]*	{ \a len -> return $ Fun (tokenStr a len) }

  -- read symbol
  $symbol			{ \a len -> return $ Sym (head $ tokenStr a len) }



{
-- Each action has type :: String -> Token

-- | The token type:
data Token =
	Fun String	|
	Int Int         |
	Var String	|
	Newline Int	|
	Sym Char	|
	Comment String	|
	Sep		|
	TermSep		|
	ClauseTer	|
	Eof
	deriving (Eq,Show)

-- | Line extraction helper
line :: AlexInput -> Int
line ((AlexPn _ l _), _, _, _) = l

-- | Token extraction helper
tokenStr :: AlexInput -> Int -> String
tokenStr (_, _, _, s) len = take len s

--scanTokens' :: String -> [Token]
--scanTokens' = \s -> alexScanTokens s ++ [Eof]

scanTokens :: (Token -> Alex a) -> Alex a
scanTokens = (alexMonadScan >>=)

alexEOF = return Eof


}
