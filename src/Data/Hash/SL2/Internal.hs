module Data.Hash.SL2.Internal
  ( module Data.Hash.SL2.Internal
  , module Data.Hash.SL2.Internal.Hash
  ) where

import Foreign
import System.IO.Unsafe

import Data.Hash.SL2.Internal.Hash
import Data.Hash.SL2.Unsafe

import Data.Functor

withHashPtrNew :: (Ptr Hash -> IO a) -> IO (Hash, a)
withHashPtrNew f = mallocForeignPtrBytes tzHashSize >>= \fp -> (\r -> (H fp, r)) <$> withForeignPtr fp (f . castPtr)

withHashPtrCopy :: Hash -> (Ptr Hash -> IO a) -> IO (Hash, a)
withHashPtrCopy h f = unsafeUseAsPtr h $ \hp -> withHashPtrNew $ \hp' -> copyBytes hp' hp tzHashSize >> f hp'

fromBytes :: [Word8] -> Hash
fromBytes ws = H $ unsafePerformIO $ do
  fp <- mallocForeignPtrArray0 tzHashSize
  withForeignPtr fp $ \p ->
    mapM_ (\(w, off) -> pokeElemOff p off w) (zip ws [0..tzHashSize-1])
  return (castForeignPtr fp)
