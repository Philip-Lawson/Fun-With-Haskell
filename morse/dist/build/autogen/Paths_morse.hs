module Paths_morse (
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

bindir     = "/Users/home/Library/Haskell/bin"
libdir     = "/Users/home/Library/Haskell/ghc-7.10.2-x86_64/lib/morse-0.1.0.0"
datadir    = "/Users/home/Library/Haskell/share/ghc-7.10.2-x86_64/morse-0.1.0.0"
libexecdir = "/Users/home/Library/Haskell/libexec"
sysconfdir = "/Users/home/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "morse_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "morse_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "morse_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "morse_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "morse_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
