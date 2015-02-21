{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Data.Hash.SL2.Internal where

import Foreign.Safe
import Foreign.C.Types

newtype Hash = H (ForeignPtr ())

hashSize = 64 :: Int
hashLen = 86 :: Int

foreign import capi "sl2-inl.h sl2_eq"
  eq :: Ptr Hash -> Ptr Hash -> IO CInt

foreign import capi "sl2-inl.h sl2_unit"
  unit :: Ptr Hash -> IO ()

foreign import capi "sl2-inl.h sl2_mul_buf_right"
  append :: Ptr Hash -> Ptr CChar -> CSize -> IO ()

foreign import capi "sl2-inl.h sl2_mul_buf_left"
  prepend :: Ptr Hash -> Ptr CChar -> CSize -> IO ()

foreign import capi "sl2-inl.h sl2_mul"
  concat :: Ptr Hash -> Ptr Hash -> Ptr Hash -> IO ()

foreign import capi "sl2-inl.h sl2_serialize"
  serialize :: Ptr Hash -> Ptr CChar -> IO ()

foreign import capi "sl2-inl.h sl2_unserialize"
  unserialize :: Ptr Hash -> Ptr CChar -> IO ()