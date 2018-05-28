module Paths_lcbf (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ali/.cabal/bin"
libdir     = "/home/ali/.cabal/lib/x86_64-linux-ghc-7.10.3/lcbf-0.1.0.0-5ULFEpm2QGC9PR2CuHxHeO"
datadir    = "/home/ali/.cabal/share/x86_64-linux-ghc-7.10.3/lcbf-0.1.0.0"
libexecdir = "/home/ali/.cabal/libexec"
sysconfdir = "/home/ali/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lcbf_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lcbf_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lcbf_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lcbf_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lcbf_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
