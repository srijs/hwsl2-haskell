{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Hash.SL2.Internal.Imports where

import Foreign.Ptr
import Foreign.C.Types

import Data.Hash.SL2.Internal

foreign import ccall "tillich-zemor.h tz_hash_eq"
  eq :: Ptr Hash -> Ptr Hash -> IO CInt

foreign import ccall "tillich-zemor.h tz_hash_unit"
  unit :: Ptr Hash -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_append"
  append :: Ptr Hash -> Ptr CChar -> CSize -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_prepend"
  prepend :: Ptr Hash -> Ptr CChar -> CSize -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_concat"
  concat :: Ptr Hash -> Ptr Hash -> Ptr Hash -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_serialize"
  serialize :: Ptr Hash -> Ptr CChar -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_unserialize"
  unserialize :: Ptr Hash -> Ptr CChar -> IO ()

