{-# LANGUAGE ForeignFunctionInterface #-}
{-|
FFI bindings to the ACI library.
-}
module ACI.Internal where

import Prelude

import Foreign.C.Types
import Foreign.Ptr

foreign import ccall safe "aci.h" 
  trainIdentity :: Ptr (Ptr CChar) -> Ptr CInt -> CInt -> IO (Ptr a)

foreign import ccall safe "aci.h"
  predictIdentity :: Ptr a -> Ptr CChar -> IO CInt

foreign import ccall safe "aci.h"
  saveIdentityClassifier :: Ptr a -> Ptr CChar -> IO ()

foreign import ccall safe "aci.h"
  loadIdentityClassifier :: Ptr CChar -> IO (Ptr a)

foreign import ccall safe "aci.h"
  freeIdentityClassifier :: Ptr a -> IO ()
