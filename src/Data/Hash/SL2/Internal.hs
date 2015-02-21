module Data.Hash.SL2.Internal where

import Foreign

newtype Hash = H (ForeignPtr ())

hashSize = 64 :: Int
hashLen = 86 :: Int