{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_csvdsl (
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
bindir     = "C:\\Users\\Lauren\\Documents\\compsci part 2\\plc_coursework\\PLC-Group-53\\csvdsl\\.stack-work\\install\\e6356895\\bin"
libdir     = "C:\\Users\\Lauren\\Documents\\compsci part 2\\plc_coursework\\PLC-Group-53\\csvdsl\\.stack-work\\install\\e6356895\\lib\\x86_64-windows-ghc-9.8.4\\csvdsl-0.1.0.0-SzMjuNHZCP6Hy9BF8aTeY-csvdsl"
dynlibdir  = "C:\\Users\\Lauren\\Documents\\compsci part 2\\plc_coursework\\PLC-Group-53\\csvdsl\\.stack-work\\install\\e6356895\\lib\\x86_64-windows-ghc-9.8.4"
datadir    = "C:\\Users\\Lauren\\Documents\\compsci part 2\\plc_coursework\\PLC-Group-53\\csvdsl\\.stack-work\\install\\e6356895\\share\\x86_64-windows-ghc-9.8.4\\csvdsl-0.1.0.0"
libexecdir = "C:\\Users\\Lauren\\Documents\\compsci part 2\\plc_coursework\\PLC-Group-53\\csvdsl\\.stack-work\\install\\e6356895\\libexec\\x86_64-windows-ghc-9.8.4\\csvdsl-0.1.0.0"
sysconfdir = "C:\\Users\\Lauren\\Documents\\compsci part 2\\plc_coursework\\PLC-Group-53\\csvdsl\\.stack-work\\install\\e6356895\\etc"

getBinDir     = catchIO (getEnv "csvdsl_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "csvdsl_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "csvdsl_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "csvdsl_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "csvdsl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "csvdsl_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
