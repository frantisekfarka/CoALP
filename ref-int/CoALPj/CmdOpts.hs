{-# LANGUAGE Arrows #-}

-- | Command line option parser
module CoALPj.CmdOpts (
	
	  runArgParser
	, Opt
) where

import Control.Applicative
import Data.Text

import Options.Applicative 
import Options.Applicative.Arrows ()
import qualified Text.PrettyPrint.ANSI.Leijen as PP

-- | Argument parser for command line options
--runArgParser :: IO [Opt]
runArgParser :: IO CmdOpts
runArgParser = execParser $ info parseOptions fullDesc

-- | Program options datatype
--
-- ??
data Opt = OptVerbose	-- ^ some option 1
	| OptVersion	-- ^ some option 2
	| OptDummy1 Int
	| OptDummy2 String
	deriving (Show)

-- | Command line optins
--
-- ??
data CmdOpts = CmdOpts {
	  optVerbose :: Bool
	, optVersion :: Bool
	, optDummy1 :: Int
	, optDummy2 :: String
	}
	deriving (Show)


-- | Parser description
--parseOptions :: Parser [Opt]
parseOptions :: Parser CmdOpts
parseOptions = CmdOpts  <$> --many $
	switch (
		short 'v' 
		<> long "verbose"
		<> help "Verbose output" 
		<> helpDoc (Just (PP.text "hello PP.text world"))
		)
	<*> switch	(short 'V' <>	long "version" <> help "Show version")
	<*> option auto	(long "dummy2" <> help "Dummy Int" <> value 7)
	<*> strOption	(long "dummy1" <> help "Dummy String" <> value "FooBar")




