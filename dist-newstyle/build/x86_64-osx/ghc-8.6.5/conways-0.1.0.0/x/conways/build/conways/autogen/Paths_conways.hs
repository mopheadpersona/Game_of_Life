{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_conways (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/deemaneken/.cabal/bin"
libdir     = "/Users/deemaneken/.cabal/lib/x86_64-osx-ghc-8.6.5/conways-0.1.0.0-inplace-conways"
dynlibdir  = "/Users/deemaneken/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/deemaneken/.cabal/share/x86_64-osx-ghc-8.6.5/conways-0.1.0.0"
libexecdir = "/Users/deemaneken/.cabal/libexec/x86_64-osx-ghc-8.6.5/conways-0.1.0.0"
sysconfdir = "/Users/deemaneken/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "conways_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "conways_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "conways_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "conways_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "conways_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "conways_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
