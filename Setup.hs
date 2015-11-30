{-# LANGUAGE RecordWildCards #-}

import Prelude hiding ((<$>))

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Maybe (fromMaybe, isJust)

import System.Directory hiding (makeAbsolute, withCurrentDirectory)
import System.FilePath

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity

main :: IO ()
main = defaultMainWithHooks customHooks
  where
    customHooks = simpleUserHooks { postConf = customPostConf (postConf simpleUserHooks)
                                  , buildHook = customBuildHook (buildHook simpleUserHooks)
                                  , replHook = customReplHook (replHook simpleUserHooks)
                                  , preSDist = customPreSDist (preSDist simpleUserHooks)
                                  }

customPostConf :: (Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ())
               -> Args
               -> ConfigFlags
               -> PackageDescription
               -> LocalBuildInfo
               -> IO ()
customPostConf innerHook args configFlags packageDescription@PackageDescription{..} =
    innerHook args configFlags packageDescription'
  where
    packageDescription' = case library of
        Nothing -> packageDescription
        Just library'@Library{..} -> packageDescription {
            library = Just library' {
                libBuildInfo = libBuildInfo {
                    extraLibs = filter ("reedsolomon" /=) (extraLibs libBuildInfo)
                }
            }
        }

data LibraryType = Static | Shared
  deriving (Show, Eq)

buildLibrary :: Verbosity -> FilePath -> LibraryType -> IO FilePath
buildLibrary verbosity buildDir libType = do
    root <- makeAbsolute =<< getCurrentDirectory
    absBuildDir <- makeAbsolute buildDir
    let cbitsBuildDir = absBuildDir </> "cbits"
        suffix = case libType of
                    Static -> "static"
                    Shared -> "shared"
        targetDir = cbitsBuildDir </> suffix
        configure = root </> "cbits" </> "configure"

    configureExists <- doesFileExist configure
    unless configureExists $
        die $ concat [ "'configure' script not found. "
                     , "If this is not a release version, you probably need "
                     , "to run 'autoreconf -i' in the 'cbits' directory to generate it."
                     ]

    createDirectoryIfMissing True targetDir

    hasMakefile <- doesFileExist (targetDir </> "Makefile")
    unless hasMakefile $
        withCurrentDirectory targetDir $ do
            let libOptions = case libType of
                    Static -> ["--enable-static", "--disable-shared"]
                    Shared -> ["--disable-static", "--enable-shared"]
                fromWindows = map (\c -> if c == '\\' then '/' else c)
            rawSystemExit verbosity "sh" $ [ fromWindows configure
                                           , "--libdir=" ++ fromWindows targetDir
                                           , "--with-pic"
                                           ] ++ libOptions

    rawSystemExit verbosity "make" ["-C", targetDir, "--no-print-directory", "install"]

    return targetDir

setupLibrary :: Verbosity
             -> FilePath
             -> ConfigFlags
             -> LibraryType
             -> PackageDescription
             -> IO PackageDescription
setupLibrary verbosity buildDir configFlags buildType packageDescription = do
    let wantSIMD = fromMaybe True $ lookup (FlagName "simd") $ configConfigurationsFlags configFlags

    if not wantSIMD
    then return packageDescription
    else do
        libDir <- buildLibrary verbosity buildDir buildType

        let updateBuildInfo buildInfo = buildInfo {
              extraLibDirs = libDir : extraLibDirs buildInfo
            , includeDirs = libDir : includeDirs buildInfo
            }

        return $ packageDescription {
              library = fmap (\l -> l { libBuildInfo = updateBuildInfo (libBuildInfo l) }) (library packageDescription)
            , executables = map (\e -> e { buildInfo = updateBuildInfo (buildInfo e) }) (executables packageDescription)
            , testSuites = map (\t -> t { testBuildInfo = updateBuildInfo (testBuildInfo t) }) (testSuites packageDescription)
            , benchmarks = map (\b -> b { benchmarkBuildInfo = updateBuildInfo (benchmarkBuildInfo b) }) (benchmarks packageDescription)
            }

customBuildHook :: (PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ())
                -> PackageDescription
                -> LocalBuildInfo
                -> UserHooks
                -> BuildFlags
                -> IO ()
customBuildHook innerHook packageDescription@PackageDescription{..} localBuildInfo@LocalBuildInfo{..} userHooks buildFlags = do
    packageDescription' <- setupLibrary (fromFlag $ buildVerbosity buildFlags) buildDir configFlags Static packageDescription

    innerHook packageDescription' localBuildInfo userHooks buildFlags

customReplHook :: (PackageDescription -> LocalBuildInfo -> UserHooks -> ReplFlags -> [String] -> IO ())
               -> PackageDescription
               -> LocalBuildInfo
               -> UserHooks
               -> ReplFlags
               -> [String]
               -> IO ()
customReplHook innerHook packageDescription@PackageDescription{..} localBuildInfo@LocalBuildInfo{..} userHooks replFlags strings = do
    packageDescription' <- setupLibrary (fromFlag $ replVerbosity replFlags) buildDir configFlags Shared packageDescription

    innerHook packageDescription' localBuildInfo userHooks replFlags strings

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute = (normalise <$>) . absolutize
  where absolutize path -- avoid the call to `getCurrentDirectory` if we can
          | isRelative path = (</> path) . addTrailingPathSeparator <$>
                              getCurrentDirectory
          | otherwise       = return path

withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

customPreSDist :: (Args -> SDistFlags -> IO HookedBuildInfo)
               -> Args
               -> SDistFlags
               -> IO HookedBuildInfo
customPreSDist innerHook args flags = do
    hookedBuildInfo <- innerHook args flags

    root <- makeAbsolute =<< getCurrentDirectory
    let cbits = root </> "cbits"
        verbosity = fromFlag (sDistVerbosity flags)

    withCurrentDirectory cbits $
        rawSystemExit verbosity "autoreconf" ["-i"]

    return hookedBuildInfo
