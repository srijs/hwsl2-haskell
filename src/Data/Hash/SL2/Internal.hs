module Data.Hash.SL2.Internal where

import Foreign
import System.IO.Unsafe

newtype Hash = H (ForeignPtr ())

hashSize = 64 :: Int
hashLen = 86 :: Int

fromBytes :: [Word8] -> Hash
fromBytes ws = H $ unsafePerformIO $ do
  fp <- mallocForeignPtrArray0 hashSize
  withForeignPtr fp $ \p ->
    mapM_ (\(w, off) -> pokeElemOff p off w) (zip ws [0..hashSize-1])
  return (castForeignPtr fp)
