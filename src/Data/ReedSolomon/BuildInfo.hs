{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Data.ReedSolomon.BuildInfo
-- Description : Build information about the library
-- Copyright   : (C) 2015 Nicolas Trangez
-- License     : MIT (see the file LICENSE)
-- Maintainer  : Nicolas Trangez <ikke@nicolast.be>
-- Stability   : provisional
--
-- This module exposes data gathered while the library was being built,
-- including Git revision, dependency information, tool versions, Cabal
-- flags etc.

module Data.ReedSolomon.BuildInfo (
      buildInfo
    , BuildInfo(..)
    , DependencyInfo(..)
    , ToolInfo(..)
    , FlagInfo(..)
    , GitInfo(..)
    ) where

import qualified Development.GitRev as GR

-- | Git revision information
data GitInfo = GitInfo { gitRevision :: String  -- ^ Git revision hash
                       , gitBranch :: String  -- ^ Git branch
                       , gitDirty :: Bool  -- ^ State of the tree
                       }
  deriving (Show, Eq)

mkGitInfo :: Maybe GitInfo
mkGitInfo = case gitRevision info of
    "UNKNOWN" -> Nothing
    _ -> Just info
  where
    info = GitInfo { gitRevision = $(GR.gitHash)
                   , gitBranch = $(GR.gitBranch)
                   , gitDirty = $(GR.gitDirty)
                   }

-- | Cabal flag settings
data FlagInfo = FlagInfo { flagSIMD :: Bool  -- ^ SIMD flag was set
                         , flagLLVM :: Bool  -- ^ LLVM flag was set
                         }
  deriving (Show, Eq)

mkFlagInfo :: FlagInfo
mkFlagInfo = FlagInfo{..}
  where
#ifdef SIMD
    flagSIMD = True
#else
    flagSIMD = False
#endif

#ifdef LLVM
    flagLLVM = True
#else
    flagLLVM = False
#endif

-- | Build tool versions
--
-- Note: this doesn't include GHC version, which can be queried through
-- e.g. invocation with `+RTS --info`.
data ToolInfo = ToolInfo { toolGCC :: Maybe String  -- ^ GCC version
                         , toolLLVM :: Maybe Int  -- ^ LLVM version identifier
                         }
  deriving (Show, Eq)

mkToolInfo :: ToolInfo
mkToolInfo = ToolInfo{..}
  where
#ifdef TOOL_VERSION_gcc
    toolGCC = Just TOOL_VERSION_gcc
#else
    toolGCC = Nothing
#endif

#ifdef __GLASGOW_HASKELL_LLVM__
    toolLLVM = Just  __GLASGOW_HASKELL_LLVM__
#else
    toolLLVM = Nothing
#endif

-- | Dependency version information
data DependencyInfo = DependencyInfo { depBase :: String
                                     , depVector :: String
                                     , depLoop :: String
                                     , depPrimitive :: String
                                     , depMtl :: String
                                     , depExceptions :: String
                                     , depBytestring :: String
                                     , depProfunctors :: String
                                     }
  deriving (Show, Eq)

mkDependencyInfo :: DependencyInfo
mkDependencyInfo = DependencyInfo{..}
  where
    depBase = VERSION_base
    depVector = VERSION_vector
    depLoop = VERSION_loop
    depPrimitive = VERSION_primitive
    depMtl = VERSION_mtl
    depExceptions = VERSION_exceptions
    depBytestring = VERSION_bytestring
    depProfunctors = VERSION_profunctors

-- | Build information structure
data BuildInfo = BuildInfo { git :: Maybe GitInfo  -- ^ Git tree information, if built from Git
                           , flags :: FlagInfo  -- ^ Cabal flag settings
                           , tools :: ToolInfo  -- ^ Build tool versions
                           , dependencies :: DependencyInfo  -- ^ Dependency version information
                           }
  deriving (Show, Eq)

-- | Build information
buildInfo :: BuildInfo
buildInfo = BuildInfo{..}
  where
    git = mkGitInfo
    flags = mkFlagInfo
    tools = mkToolInfo
    dependencies = mkDependencyInfo
