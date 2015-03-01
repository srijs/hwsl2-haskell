{-# LANGUAGE Unsafe #-}

module Data.Hash.SL2.Unsafe (unsafeUseAsPtr, unsafeUseAsPtr2, unsafeWithNew, unsafePack, unsafeUnpack) where

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

unsafePack :: Storable a => [a] -> Hash
unsafePack as@(a:_) = H $ unsafePerformIO $ do
  let len = hashSize `div` sizeOf a
  fp <- mallocForeignPtrArray0 len
  withForeignPtr fp $ \p ->
    mapM_ (\(a, off) -> pokeElemOff p off a) (zip as [0..len-1])
  return (castForeignPtr fp)

unsafeUnpack :: Storable a => Hash -> [a]
unsafeUnpack h = unsafePerformIO $ unsafeUseAsPtr h $ rec 0
  where rec off _ | off >= hashSize = return []
        rec off p = do
          a <- peekByteOff (castPtr p) off
          r <- rec (off + sizeOf a) p
          return (a : r)