module Paths_compiler (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Users\\Maarten\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Maarten\\AppData\\Roaming\\cabal\\compiler-0.1\\ghc-7.6.3"
datadir    = "C:\\Users\\Maarten\\AppData\\Roaming\\cabal\\compiler-0.1"
libexecdir = "C:\\Users\\Maarten\\AppData\\Roaming\\cabal\\compiler-0.1"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "compiler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "compiler_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "compiler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "compiler_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
