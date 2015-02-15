{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Hash.SL2.Internal.Imports where

import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "tillich-zemor.h tz_hash_eq"
  eq :: Ptr () -> Ptr () -> IO CInt

foreign import ccall "tillich-zemor.h tz_hash_unit"
  unit :: Ptr () -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_append"
  append :: Ptr () -> Ptr CChar -> CSize -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_prepend"
  prepend :: Ptr () -> Ptr CChar -> CSize -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_concat"
  concat :: Ptr () -> Ptr () -> Ptr () -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_serialize"
  serialize :: Ptr () -> Ptr CChar -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_unserialize"
  unserialize :: Ptr () -> Ptr CChar -> IO ()

