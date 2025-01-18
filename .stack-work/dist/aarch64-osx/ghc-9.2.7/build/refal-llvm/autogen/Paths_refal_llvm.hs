{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_refal_llvm (
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
bindir     = "/Users/noplana/Desktop/ /sem5/\1060\1055/\1087\1088\1086\1077\1082\1090/refal-llvm/.stack-work/install/aarch64-osx/51147ab1c5affd103697ab15e8d6e4d1961dcb97f8d60a715edb6b1b99e94353/9.2.7/bin"
libdir     = "/Users/noplana/Desktop/ /sem5/\1060\1055/\1087\1088\1086\1077\1082\1090/refal-llvm/.stack-work/install/aarch64-osx/51147ab1c5affd103697ab15e8d6e4d1961dcb97f8d60a715edb6b1b99e94353/9.2.7/lib/aarch64-osx-ghc-9.2.7/refal-llvm-0.1.0.0-4IISHWsaqPG8ixUMi85yZh-refal-llvm"
dynlibdir  = "/Users/noplana/Desktop/ /sem5/\1060\1055/\1087\1088\1086\1077\1082\1090/refal-llvm/.stack-work/install/aarch64-osx/51147ab1c5affd103697ab15e8d6e4d1961dcb97f8d60a715edb6b1b99e94353/9.2.7/lib/aarch64-osx-ghc-9.2.7"
datadir    = "/Users/noplana/Desktop/ /sem5/\1060\1055/\1087\1088\1086\1077\1082\1090/refal-llvm/.stack-work/install/aarch64-osx/51147ab1c5affd103697ab15e8d6e4d1961dcb97f8d60a715edb6b1b99e94353/9.2.7/share/aarch64-osx-ghc-9.2.7/refal-llvm-0.1.0.0"
libexecdir = "/Users/noplana/Desktop/ /sem5/\1060\1055/\1087\1088\1086\1077\1082\1090/refal-llvm/.stack-work/install/aarch64-osx/51147ab1c5affd103697ab15e8d6e4d1961dcb97f8d60a715edb6b1b99e94353/9.2.7/libexec/aarch64-osx-ghc-9.2.7/refal-llvm-0.1.0.0"
sysconfdir = "/Users/noplana/Desktop/ /sem5/\1060\1055/\1087\1088\1086\1077\1082\1090/refal-llvm/.stack-work/install/aarch64-osx/51147ab1c5affd103697ab15e8d6e4d1961dcb97f8d60a715edb6b1b99e94353/9.2.7/etc"

getBinDir     = catchIO (getEnv "refal_llvm_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "refal_llvm_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "refal_llvm_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "refal_llvm_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "refal_llvm_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "refal_llvm_sysconfdir") (\_ -> return sysconfdir)




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
