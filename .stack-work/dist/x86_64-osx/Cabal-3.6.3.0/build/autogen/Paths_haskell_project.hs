{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_haskell_project (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/chandrimamukherjee/Documents/gitrepos/haskell-project/.stack-work/install/x86_64-osx/63ebe885855d2fb875b8c3821eb76d1366ffe8d2a12cf5fd87a56478a404af97/9.2.5/bin"
libdir     = "/Users/chandrimamukherjee/Documents/gitrepos/haskell-project/.stack-work/install/x86_64-osx/63ebe885855d2fb875b8c3821eb76d1366ffe8d2a12cf5fd87a56478a404af97/9.2.5/lib/x86_64-osx-ghc-9.2.5/haskell-project-0.1.0.0-ABiGtIxDzjc3YCexQpsxhj"
dynlibdir  = "/Users/chandrimamukherjee/Documents/gitrepos/haskell-project/.stack-work/install/x86_64-osx/63ebe885855d2fb875b8c3821eb76d1366ffe8d2a12cf5fd87a56478a404af97/9.2.5/lib/x86_64-osx-ghc-9.2.5"
datadir    = "/Users/chandrimamukherjee/Documents/gitrepos/haskell-project/.stack-work/install/x86_64-osx/63ebe885855d2fb875b8c3821eb76d1366ffe8d2a12cf5fd87a56478a404af97/9.2.5/share/x86_64-osx-ghc-9.2.5/haskell-project-0.1.0.0"
libexecdir = "/Users/chandrimamukherjee/Documents/gitrepos/haskell-project/.stack-work/install/x86_64-osx/63ebe885855d2fb875b8c3821eb76d1366ffe8d2a12cf5fd87a56478a404af97/9.2.5/libexec/x86_64-osx-ghc-9.2.5/haskell-project-0.1.0.0"
sysconfdir = "/Users/chandrimamukherjee/Documents/gitrepos/haskell-project/.stack-work/install/x86_64-osx/63ebe885855d2fb875b8c3821eb76d1366ffe8d2a12cf5fd87a56478a404af97/9.2.5/etc"

getBinDir     = catchIO (getEnv "haskell_project_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "haskell_project_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "haskell_project_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "haskell_project_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_project_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
