{-# LANGUAGE ForeignFunctionInterface, CApiFFI #-}

module CPython.GhciHack
  ( globallyLoadLibPython
  , initializeInGhci
  ) where

import Foreign.C
import Foreign.Ptr (nullPtr, Ptr)
import Control.Monad (void)
import Data.Bits ((.|.))

import CPython (initialize)

foreign import capi "dlfcn.h dlerror" cDlerror :: IO CString
foreign import capi "dlfcn.h dlopen"  cDlopen  :: CString -> CInt -> IO (Ptr ())

foreign import capi "dlfcn.h value RTLD_LAZY"   rtld_lazy   :: CInt
foreign import capi "dlfcn.h value RTLD_GLOBAL" rtld_global :: CInt

libPythonName :: String
libPythonName = "libpython3.so"

dlOpenLazyGlobal :: CInt
dlOpenLazyGlobal = rtld_lazy .|. rtld_global

-- | GHCi uses @dlopen@ with the @RTLD_LOCAL@ flag for dynamic loading
-- (see also @internal_dlopen@ and the note @[RTLD_LOCAL]@ in @rts/Linker.c@).
-- However, Python
-- [documented in version 3.8](https://docs.python.org/3/whatsnew/3.8.html#changes-in-the-c-api)
-- that \"libpython must not be loaded with @RTLD_LOCAL@, but @RTLD_GLOBAL@ instead\".
-- This seems to make GHCi incompatible with this package.
-- 
-- To circumvent this restriction, we provide this function as a
-- dirty hack: call it from within GHCi after 'CPython.initialize'
-- but before using anything else.
-- 
-- It returns @Nothing@ on success and @Just error_msg@ otherwise.
globallyLoadLibPython :: IO (Maybe String)
globallyLoadLibPython = do
  lib <- newCString libPythonName
  void $ cDlerror  -- clear dlerror message
  void $ cDlopen lib dlOpenLazyGlobal
  may_err <- cDlerror
  if may_err == nullPtr
  then return Nothing  -- no error, dlopen was successful
  else peekCString may_err >>= return . Just

-- | Convenience function for initializing in GHCi. Runs
-- 'CPython.initialize' followed by 'globallyLoadLibPython'.
initializeInGhci :: IO (Maybe String)
initializeInGhci = initialize >> globallyLoadLibPython
