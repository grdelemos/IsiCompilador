{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_projeto (
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

bindir     = "/mnt/d/Downloads/Compiladores/Entrega comp/IsiCompilador/.stack-work/install/x86_64-linux-tinfo6/4017edb3972821eba7796e7ecdf3da164e3c412d7393713269ba29b9efd7cc8e/9.0.2/bin"
libdir     = "/mnt/d/Downloads/Compiladores/Entrega comp/IsiCompilador/.stack-work/install/x86_64-linux-tinfo6/4017edb3972821eba7796e7ecdf3da164e3c412d7393713269ba29b9efd7cc8e/9.0.2/lib/x86_64-linux-ghc-9.0.2/projeto-0.1.0.0-AKVSZ1grPx0E4lriNkae0Y-projeto"
dynlibdir  = "/mnt/d/Downloads/Compiladores/Entrega comp/IsiCompilador/.stack-work/install/x86_64-linux-tinfo6/4017edb3972821eba7796e7ecdf3da164e3c412d7393713269ba29b9efd7cc8e/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/mnt/d/Downloads/Compiladores/Entrega comp/IsiCompilador/.stack-work/install/x86_64-linux-tinfo6/4017edb3972821eba7796e7ecdf3da164e3c412d7393713269ba29b9efd7cc8e/9.0.2/share/x86_64-linux-ghc-9.0.2/projeto-0.1.0.0"
libexecdir = "/mnt/d/Downloads/Compiladores/Entrega comp/IsiCompilador/.stack-work/install/x86_64-linux-tinfo6/4017edb3972821eba7796e7ecdf3da164e3c412d7393713269ba29b9efd7cc8e/9.0.2/libexec/x86_64-linux-ghc-9.0.2/projeto-0.1.0.0"
sysconfdir = "/mnt/d/Downloads/Compiladores/Entrega comp/IsiCompilador/.stack-work/install/x86_64-linux-tinfo6/4017edb3972821eba7796e7ecdf3da164e3c412d7393713269ba29b9efd7cc8e/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "projeto_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "projeto_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "projeto_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "projeto_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "projeto_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "projeto_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
