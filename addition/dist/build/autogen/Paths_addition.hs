module Paths_addition (
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

bindir     = "/Users/home/Documents/Sandbox/Fun-With-Haskell/addition/.cabal-sandbox/bin"
libdir     = "/Users/home/Documents/Sandbox/Fun-With-Haskell/addition/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.2/addition-0.1.0.0-KIp16Cx9GU9LnUGrDNT3WS"
datadir    = "/Users/home/Documents/Sandbox/Fun-With-Haskell/addition/.cabal-sandbox/share/x86_64-osx-ghc-7.10.2/addition-0.1.0.0"
libexecdir = "/Users/home/Documents/Sandbox/Fun-With-Haskell/addition/.cabal-sandbox/libexec"
sysconfdir = "/Users/home/Documents/Sandbox/Fun-With-Haskell/addition/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "addition_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "addition_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "addition_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "addition_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "addition_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
