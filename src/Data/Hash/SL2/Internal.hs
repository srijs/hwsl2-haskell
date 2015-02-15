module Data.Hash.SL2.Internal where

import Foreign
import Foreign.C.Types
import System.IO.Unsafe

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe

import qualified Data.Hash.SL2.Internal.Imports as Imports

import Data.Functor

-- Mutable Helpers

append :: ByteString -> Ptr () -> IO ()
append s p = unsafeUseAsCStringLen s $ \(s', len) -> Imports.append p s' (fromIntegral len)

prepend :: ByteString -> Ptr () -> IO ()
prepend s p = unsafeUseAsCStringLen s $ \(s', len) -> Imports.prepend p s' (fromIntegral len)

-- | Opaque representation of a 512 bit hash.
newtype Hash = H (ForeignPtr ())

-- Foreign Pointer Helpers

tzHashSize = 64 :: Int
tzHashLen = 86 :: Int

withHashPtr :: Hash -> (Ptr () -> IO a) -> IO a
withHashPtr (H fp) = withForeignPtr fp

withHashPtr2 :: Hash -> Hash -> (Ptr () -> Ptr () -> IO a) -> IO a
withHashPtr2 a b f = withHashPtr a (withHashPtr b . f)

withHashPtrNew :: (Ptr () -> IO a) -> IO (Hash, a)
withHashPtrNew f = mallocForeignPtrBytes tzHashSize >>= \fp -> (\r -> (H fp, r)) <$> withForeignPtr fp f

withHashPtrCopy :: Hash -> (Ptr () -> IO a) -> IO (Hash, a)
withHashPtrCopy h f = withHashPtr h $ \hp -> withHashPtrNew $ \hp' -> copyBytes hp' hp tzHashSize >> f hp'

fromBytes :: [Word8] -> Hash
fromBytes ws = H $ unsafePerformIO $ do
  fp <- mallocForeignPtrArray0 tzHashSize
  withForeignPtr fp $ \p ->
    mapM_ (\(w, off) -> pokeElemOff p off w) (zip ws [0..tzHashSize-1])
  return (castForeignPtr fp)
