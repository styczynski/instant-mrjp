{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_instant_lang (
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
bindir     = "/home/students/inf/p/ps386038/Code/instant-mrjp/.stack-work/install/x86_64-linux-tinfo6/3051282c23854dca88a143eff839c2bd0a976d72d2c74d2a60a6dc62d08d394d/9.4.7/bin"
libdir     = "/home/students/inf/p/ps386038/Code/instant-mrjp/.stack-work/install/x86_64-linux-tinfo6/3051282c23854dca88a143eff839c2bd0a976d72d2c74d2a60a6dc62d08d394d/9.4.7/lib/x86_64-linux-ghc-9.4.7/instant-lang-0.1.0.0-F6yoq7A1VNRG6TBR5TnUYh-insc_llvm"
dynlibdir  = "/home/students/inf/p/ps386038/Code/instant-mrjp/.stack-work/install/x86_64-linux-tinfo6/3051282c23854dca88a143eff839c2bd0a976d72d2c74d2a60a6dc62d08d394d/9.4.7/lib/x86_64-linux-ghc-9.4.7"
datadir    = "/home/students/inf/p/ps386038/Code/instant-mrjp/.stack-work/install/x86_64-linux-tinfo6/3051282c23854dca88a143eff839c2bd0a976d72d2c74d2a60a6dc62d08d394d/9.4.7/share/x86_64-linux-ghc-9.4.7/instant-lang-0.1.0.0"
libexecdir = "/home/students/inf/p/ps386038/Code/instant-mrjp/.stack-work/install/x86_64-linux-tinfo6/3051282c23854dca88a143eff839c2bd0a976d72d2c74d2a60a6dc62d08d394d/9.4.7/libexec/x86_64-linux-ghc-9.4.7/instant-lang-0.1.0.0"
sysconfdir = "/home/students/inf/p/ps386038/Code/instant-mrjp/.stack-work/install/x86_64-linux-tinfo6/3051282c23854dca88a143eff839c2bd0a976d72d2c74d2a60a6dc62d08d394d/9.4.7/etc"

getBinDir     = catchIO (getEnv "instant_lang_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "instant_lang_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "instant_lang_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "instant_lang_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "instant_lang_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "instant_lang_sysconfdir") (\_ -> return sysconfdir)




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
