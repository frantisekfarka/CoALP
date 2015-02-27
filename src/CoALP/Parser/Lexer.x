{
module CoALP.Parser.Lexer (scanTokens) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				{ \s -> Comment s}
  let					{ \s -> Let }
  in					{ \s -> In }
  $digit+				{ \s -> Int (read s) }
  [\=\+\-\*\/\(\)]			{ \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']*		{ \s -> Var s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
	Let 		|
	In  		|
	Sym Char	|
	Var String	|
	Int Int         |
	Comment String
	deriving (Eq,Show)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens

}
