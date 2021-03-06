module Paths_hello_haskell (
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

bindir     = "/Users/home/Documents/Sandbox/Fun-With-Haskell/hello/.cabal-sandbox/bin"
libdir     = "/Users/home/Documents/Sandbox/Fun-With-Haskell/hello/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.2/hello-haskell-0.1.0.0-5RuYAhkvOux3g8ykHX3B0J"
datadir    = "/Users/home/Documents/Sandbox/Fun-With-Haskell/hello/.cabal-sandbox/share/x86_64-osx-ghc-7.10.2/hello-haskell-0.1.0.0"
libexecdir = "/Users/home/Documents/Sandbox/Fun-With-Haskell/hello/.cabal-sandbox/libexec"
sysconfdir = "/Users/home/Documents/Sandbox/Fun-With-Haskell/hello/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hello_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hello_haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hello_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hello_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hello_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
