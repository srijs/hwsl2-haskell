module Data.Hash.SL2.Internal.Hash where

import Foreign.ForeignPtr (ForeignPtr)

newtype Hash = H (ForeignPtr ())

tzHashSize = 64 :: Int
tzHashLen = 86 :: Int

