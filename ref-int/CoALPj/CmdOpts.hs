{-# LANGUAGE Arrows #-}

-- | Command line option parser
module CoALPj.CmdOpts (
	
	  runArgParser
	, CmdOpts
) where

import Control.Applicative
import Data.Text

import Options.Applicative 
import Options.Applicative.Arrows
import qualified Text.PrettyPrint.ANSI.Leijen as PP


import CoALPj.REPL

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
	  optVerbose :: Bool
	--, optVersion :: Bool
	, optDummy1 :: Int
	, optDummy2 :: String
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
		<> helpDoc (Just (PP.text "hello PP.text world"))
		)
	-- <*> switch	(short 'V' <>	long "version" <> help "Show version")
	<*> option auto	(long "dummy2" <> help "Dummy Int" <> value 7)
	<*> strOption	(long "dummy1" <> help "Dummy String" <> value "FooBar")

-- | Version info option parser
--
parseVersion :: Parser (a -> a)
parseVersion = infoOption ver (short 'V' <> long "version" <> help "Print version information")



