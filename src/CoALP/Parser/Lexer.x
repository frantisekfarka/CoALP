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

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$whitenonl = [\ \t\f\v]		-- whitespace nonewline
$newline = [\n\r]		-- newline

tokens :-

  $whitenonl+ 			;
  $newline+			{ \a _ -> return (Newline (line a))  }
  "%".*				{ \a len -> return $ Comment $ tokenStr a len
				}
  			
  let				{ \_ _ -> return Let }
  in				{ \_ _ -> return In }
  $digit+			{ \a len -> return $ Int (read $ tokenStr a len) }
  [\=\+\-\*\/\(\)]		{ \a len -> return $ Sym (head $ tokenStr a len) }
  $alpha [$alpha $digit \_ \']*	{ \a len -> return $ Var (tokenStr a len) }


{
-- Each action has type :: String -> Token

-- | The token type:
data Token =
	Let 		|
	In  		|
	Int Int         |
	Var String	|
	Sym Char	|
	Newline Int	|
	Comment String	|
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
