module Data.Hash.SL2.Unsafe (unsafeUseAsPtr, unsafeUseAsPtr2, unsafeWithNew, unsafePack) where

import Foreign
import System.IO.Unsafe

import Data.Hash.SL2.Internal (Hash(H), hashSize)

unsafeUseAsPtr :: Hash -> (Ptr Hash -> IO a) -> IO a
{-# INLINE unsafeUseAsPtr #-}
unsafeUseAsPtr (H fp) f = withForeignPtr fp (f . castPtr)

unsafeUseAsPtr2 :: Hash -> Hash -> (Ptr Hash -> Ptr Hash -> IO a) -> IO a
{-# INLINE unsafeUseAsPtr2 #-}
unsafeUseAsPtr2 a b f = unsafeUseAsPtr a (unsafeUseAsPtr b . f)

unsafeWithNew :: (Ptr Hash -> IO a) -> IO (Hash, a)
{-# INLINE unsafeWithNew #-}
unsafeWithNew f = mallocForeignPtrBytes hashSize >>= \fp -> (\r -> (H fp, r)) `fmap` withForeignPtr fp (f . castPtr)

unsafePack :: [Word8] -> Hash
unsafePack ws = H $ unsafePerformIO $ do
  fp <- mallocForeignPtrArray0 hashSize
  withForeignPtr fp $ \p ->
    mapM_ (\(w, off) -> pokeElemOff p off w) (zip ws [0..hashSize-1])
  return (castForeignPtr fp)