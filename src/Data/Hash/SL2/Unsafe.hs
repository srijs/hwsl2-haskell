{-# LANGUAGE Unsafe #-}

module Data.Hash.SL2.Unsafe (unsafeUseAsPtr, unsafeUseAsPtr2, unsafeWithNew, unsafePack) where

import Foreign.Safe
import System.IO.Unsafe

import Data.Hash.SL2.Internal (Hash(H), hashSize)

instance Storable Hash where
  sizeOf = const hashSize
  alignment = const 16
  peek p = fmap fst $ unsafeWithNew $ \hp -> copyBytes hp (castPtr p) hashSize
  poke p h = unsafeUseAsPtr h $ \hp -> copyBytes (castPtr p) hp hashSize

unsafeUseAsPtr :: Hash -> (Ptr Hash -> IO a) -> IO a
{-# INLINE unsafeUseAsPtr #-}
unsafeUseAsPtr (H fp) f = withForeignPtr fp (f . castPtr)

unsafeUseAsPtr2 :: Hash -> Hash -> (Ptr Hash -> Ptr Hash -> IO a) -> IO a
{-# INLINE unsafeUseAsPtr2 #-}
unsafeUseAsPtr2 a b f = unsafeUseAsPtr a (unsafeUseAsPtr b . f)

unsafeWithNew :: (Ptr Hash -> IO a) -> IO (Hash, a)
{-# INLINE unsafeWithNew #-}
unsafeWithNew f = mallocForeignPtr >>= \fp -> (\r -> (H (castForeignPtr fp), r)) `fmap` withForeignPtr fp f

unsafePack :: [Word8] -> Hash
unsafePack ws = H $ unsafePerformIO $ do
  fp <- mallocForeignPtrArray0 hashSize
  withForeignPtr fp $ \p ->
    mapM_ (\(w, off) -> pokeElemOff p off w) (zip ws [0..hashSize-1])
  return (castForeignPtr fp)