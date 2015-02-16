
import Control.Exception (SomeException, catch)
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import qualified Distribution.Simple.Setup as S
import Distribution.Simple.Utils
import Distribution.PackageDescription

import System.FilePath 
import qualified System.FilePath.Posix as Px
import System.Process

-- -----------------------------------------------------------------------------
-- -- Clean



-- -----------------------------------------------------------------------------
-- -- Flags
--
isRelease :: S.ConfigFlags -> Bool
isRelease flags =
    case lookup (FlagName "release") (S.configConfigurationsFlags flags) of
      Just True -> True
      Just False -> False
      Nothing -> False

-- -----------------------------------------------------------------------------
-- -- Configure

gitHash :: IO String
gitHash = do
	h <- Control.Exception.catch (readProcess "git" ["rev-parse", "--short", "HEAD"] "")
		(\e -> let e' = (e :: SomeException) in return "PRE")
	return $ takeWhile (/= '\n') h

-- Put the Git hash into a module for use in the program
-- For release builds, just put the empty string in the module
generateVersionModule verbosity dir release = do
		hash <- gitHash
		let versionModulePath = dir </> "Version_CoALP" Px.<.> "hs"
		putStrLn $ "Generating " ++ versionModulePath ++
			if release then " for release" else (" for prerelease " ++ hash) 
		createDirectoryIfMissingVerbose verbosity True dir
		rewriteFile versionModulePath (versionModuleContents hash) 

	where versionModuleContents h = "module Version_CoALP where\n\n" ++
		"gitHash :: String\n" ++
		if release
			then "gitHash = \"\"\n"
			else "gitHash = \"-git:" ++ h ++ "\"\n"


caConfigure _ flags _ local = do
		generateVersionModule verbosity (autogenModulesDir local) (isRelease (configFlags local))
	where
		verbosity = S.fromFlag $ S.configVerbosity flags

-- -----------------------------------------------------------------------------
-- -- Main
--
main = defaultMainWithHooks $ simpleUserHooks {
	--  postClean = 
	  postConf  = caConfigure
	--, postBuild = 
	--, postCopy  = 
	--, postInst  =
	--, preSDist  =
	--, postSDist =
	}

