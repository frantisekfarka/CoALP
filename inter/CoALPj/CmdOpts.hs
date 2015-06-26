{-# LANGUAGE Arrows #-}

-- | Command line option parser
module CoALPj.CmdOpts (
	  runArgParser
	, CmdOpts
	, optVerbose
	, optVVerbose
	, optQuiet
	, optDummy1
	, optDdumpLexer
	, optDdumpParser
) where

import Control.Applicative
import Data.Version (showVersion)

import Options.Applicative 
import Options.Applicative.Arrows
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Paths_CoALP
import Version_CoALP


-- | Argument parser for command line options
--runArgParser :: IO [Opt]
runArgParser :: IO CmdOpts
runArgParser = execParser ( info parser (fullDesc
		<> headerDoc   (Just caHeader)
		<> progDescDoc (Just caProgDesc)
		<> footerDoc   (Just caFooter)
	))
	where
		caHeader   = PP.hsep [
		 	  PP.text "CoALPj version"
			, PP.text ver, PP.text ", (C) 2015"
			]
		caProgDesc = PP.vsep [
		 	  PP.empty
			, PP.text "TODO some nice description here"
			]
		caFooter    = PP.vsep [
			  PP.text "See the GitHub repo"
			, PP.indent 4 $ PP.text "https://github.com/frantisekfarka"
			]



-- | Command line optins
data CmdOpts = CmdOpts {
	  optVerbose :: Bool   		-- | verbose
	, optVVerbose :: Bool  		-- | very verbose
	, optQuiet :: Bool		-- | quiet
	--, optVersion :: Bool
	, optDummy1 :: Int
	, optDdumpLexer :: Bool	-- | parser debugging output
	, optDdumpParser :: Bool	-- | parser debugging output
	}
	deriving (Show)


parser :: Parser CmdOpts
parser = runA $ proc () -> do
	opts <- asA parseOptions -< ()
	-- files <- asA (many $ argument (fmap Filename str) (metavar "FILES")) -< ()
	A parseVersion >>> A helper -< (opts)

-- | Command line options parser
parseOptions :: Parser CmdOpts
parseOptions = CmdOpts  <$> --many $
	switch (
		short 'v' 
		<> long "verbose"
		<> help "Verbose output" 
		-- <> helpDoc (Just (PP.text "hello PP.text world"))
		)
	<*> switch (
		long "vverbose"
		<> help "Very verbose output" 
		)
	<*> switch (
		short 'q'
		<> long "quiet"
		<> help "Suppres most of the output"
		)
	-- <*> switch	(short 'V' <>	long "version" <> help "Show version")
	<*> option auto	(long "dummy1" <> help "Dummy Int" <> value 7)
	<*> switch (
		long "ddump-lexer"
		<> help "Lexer debugging output"
		)
	<*> switch (
		long "ddump-parser"
		<> help "Parser debugging output"
		)

-- | Version info option parser
--
parseVersion :: Parser (a -> a)
parseVersion = infoOption v (short 'V' <> long "version" <> help "Print version information")
	where
		v = "CoALPj version " ++ ver 


-- | Version of CoALPj 
-- contains git hash in development 
ver :: String
ver = showVersion version ++ gitHash

