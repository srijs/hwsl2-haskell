{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Data.Hash.SL2.Internal where

import Prelude hiding (concat)

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.ForeignPtr
import System.IO.Unsafe

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe

import Data.Monoid
import Data.Functor
import Data.Foldable (Foldable, foldlM, foldrM)

-- Foreign Imports

foreign import ccall "tillich-zemor.h tz_hash_eq"
  tzHashEq :: Ptr () -> Ptr () -> IO CInt

foreign import ccall "tillich-zemor.h tz_hash_unit"
  tzHashUnit :: Ptr () -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_append"
  tzHashAppend :: Ptr () -> Ptr CChar -> CSize -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_prepend"
  tzHashPrepend :: Ptr () -> Ptr CChar -> CSize -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_concat"
  tzHashConcat :: Ptr () -> Ptr () -> Ptr () -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_serialize"
  tzHashSerialize :: Ptr () -> Ptr CChar -> IO ()

foreign import ccall "tillich-zemor.h tz_hash_unserialize"
  tzHashUnserialize :: Ptr () -> Ptr CChar -> IO ()

-- Mutable Helpers

append :: ByteString -> Ptr () -> IO ()
append s p = unsafeUseAsCStringLen s $ \(s', len) -> tzHashAppend p s' (fromIntegral len)

prepend :: ByteString -> Ptr () -> IO ()
prepend s p = unsafeUseAsCStringLen s $ \(s', len) -> tzHashPrepend p s' (fromIntegral len)

-- | Opaque representation of a 512 bit hash.
newtype Hash = H (ForeignPtr ())

-- Foreign Pointer Helpers

tzHashSize = 64
tzHashLen = 86

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

-- Instances

instance Storable Hash where
  sizeOf = const tzHashSize
  alignment = const 0
  peek p = fmap fst $ withHashPtrNew $ \hp -> copyBytes hp (castPtr p) tzHashSize
  poke p h = withHashPtr h $ \hp -> copyBytes (castPtr p) hp tzHashSize

instance Show Hash where
  show h = unsafePerformIO $ allocaBytes tzHashLen $ \p -> withHashPtr h (flip tzHashSerialize p) >> peekCStringLen (p, tzHashLen)

instance Eq Hash where
  a == b = toBool $ unsafePerformIO $ withHashPtr2 a b tzHashEq

instance Monoid Hash where
  mempty = fst $ unsafePerformIO $ withHashPtrNew tzHashUnit
  mappend a b = fst $ unsafePerformIO $ withHashPtrNew (withHashPtr2 a b . tzHashConcat)

-- Interface

-- | /O(n)/ Calculate the hash of the 'ByteString'. Alias for @('mempty' '<+')@.
hash :: ByteString -> Hash
hash = (<+) mempty

-- | /O(n)/ Append the hash of the 'ByteString' to the existing 'Hash'.
-- A significantly faster equivalent of @((. 'hash') . ('<>'))@.
infixl 7 <+
(<+) :: Hash -> ByteString -> Hash
(<+) h s = fst $ unsafePerformIO $ withHashPtrCopy h $ append s

-- | /O(n)/ Prepend the hash of the 'ByteString' to the existing 'Hash'.
-- A significantly faster equivalent of @(('<>') . 'hash')@.
infixr 7 +>
(+>) :: ByteString -> Hash -> Hash
(+>) s h = fst $ unsafePerformIO $ withHashPtrCopy h $ prepend s

-- | /O(n)/ Append the hash of every 'ByteString' to the existing 'Hash', from left to right.
-- A significantly faster equivalent of @('foldl' ('<+'))@.
infixl 7 <|
(<|) :: Foldable t => Hash -> t ByteString -> Hash
(<|) h ss = fst $ unsafePerformIO $ withHashPtrCopy h $ \hp -> foldlM (\p s -> p <$ append s p) hp ss

-- | /O(n)/ Prepend the hash of every 'ByteString' to the existing 'Hash', from right to left.
-- A significantly faster equivalent of @('flip' ('foldr' ('+>')))@.
infixr 7 |>
(|>) :: Foldable t => t ByteString -> Hash -> Hash
(|>) ss h = fst $ unsafePerformIO $ withHashPtrCopy h $ \hp -> foldrM (\s p -> p <$ prepend s p) hp ss

-- | /O(1)/ Parse the representation generated by 'show'.
parse :: String -> Maybe Hash
parse s = (\(h, r) -> h <$ r) $ unsafePerformIO $ withHashPtrNew $ \hp -> withCAStringLen s $ \(s', len) ->
  if len == tzHashLen then Just <$> tzHashUnserialize hp s' else return Nothing