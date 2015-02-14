{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module HwSL2 ((<+), (+>)) where

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

newtype Hash = Hash (ForeignPtr ())

tzHashSize = 64
tzHashLen = 86

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

withHashPtr :: Hash -> (Ptr () -> IO a) -> IO a
withHashPtr (Hash fp) = withForeignPtr fp

withHashPtrNew :: (Ptr () -> IO a) -> IO (Hash, a)
withHashPtrNew f = mallocForeignPtrBytes tzHashSize >>= \fp -> (\r -> (Hash fp, r)) <$> withForeignPtr fp f

withHashPtrCopy :: Hash -> (Ptr () -> IO a) -> IO (Hash, a)
withHashPtrCopy h f = withHashPtr h $ \hp -> withHashPtrNew $ \hp' -> copyBytes hp' hp tzHashSize >> f hp'

instance Show Hash where
  show h = unsafePerformIO $ withHashPtr h $ \hp -> mallocForeignPtrArray tzHashLen >>= flip withForeignPtr (\p -> tzHashSerialize hp p >> peekCStringLen (p, tzHashLen))

instance Eq Hash where
  a == b = toBool $ unsafePerformIO $ withHashPtr a $ \ap -> withHashPtr b (tzHashEq ap)

instance Monoid Hash where
  mempty = fst $ unsafePerformIO $ withHashPtrNew tzHashUnit
  mappend a b = fst $ unsafePerformIO $ withHashPtr a $ \ap -> withHashPtr b $ \bp -> withHashPtrNew (tzHashConcat ap bp)

(<+) :: Hash -> ByteString -> Hash
(<+) h s = fst $ unsafePerformIO $ unsafeUseAsCStringLen s $ \(s', len) ->
  withHashPtrCopy h $ \hp -> tzHashAppend hp s' $ fromIntegral len

(+>) :: ByteString -> Hash -> Hash
(+>) s h = fst $ unsafePerformIO $ unsafeUseAsCStringLen s $ \(s', len) ->
  withHashPtrCopy h $ \hp -> tzHashPrepend hp s' $ fromIntegral len

unserialize :: String -> Maybe Hash
unserialize s = (\(h, r) -> h <$ r) $ unsafePerformIO $ withHashPtrNew $ \hp -> withCAStringLen s $ \(s', len) ->
  if len == tzHashLen then Just <$> tzHashUnserialize hp s' else return Nothing
