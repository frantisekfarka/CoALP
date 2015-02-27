{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module CoALP.Parser.Lexer (
	  scanTokens
	, Token (..)
	) where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$whitenonl = [\ \t\f\v]		-- whitespace nonewline
$newline = [\n\r]		-- newline

tokens :-

  $whitenonl+ [\n]		;
  $newline+			{ \a s -> Newline (line a)}
  "%".*				{ \a s -> Comment s}
--  let				{ \s -> Let }
--  in				{ \s -> In }
--  $digit+			{ \s -> Int (read s) }
--  [\=\+\-\*\/\(\)]		{ \s -> Sym (head s) }
--  $alpha [$alpha $digit \_ \']*	{ \s -> Var s }


{
-- Each action has type :: String -> Token

-- | The token type:
data Token =
	Let 		|
	In  		|
	Sym Char	|
	Var String	|
	Int Int         |
	Newline Int	|
	Comment String
	deriving (Eq,Show)

-- | Line extraction helper
line :: AlexPosn -> Int
line (AlexPn _ l _) = l

scanTokens :: String -> [Token]
scanTokens = alexScanTokens

}
