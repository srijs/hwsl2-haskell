{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Data.Hash.SL2 (Hash, (<+), (+>), parse) where

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

newtype Hash = H (ForeignPtr ())

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

instance Show Hash where
  show h = unsafePerformIO $ allocaBytes tzHashLen $ \p -> withHashPtr h (flip tzHashSerialize p) >> peekCStringLen (p, tzHashLen)

instance Eq Hash where
  a == b = toBool $ unsafePerformIO $ withHashPtr2 a b tzHashEq

instance Monoid Hash where
  mempty = fst $ unsafePerformIO $ withHashPtrNew tzHashUnit
  mappend a b = fst $ unsafePerformIO $ withHashPtrNew (withHashPtr2 a b . tzHashConcat)

(<+) :: Hash -> ByteString -> Hash
(<+) h s = fst $ unsafePerformIO $ unsafeUseAsCStringLen s $ \(s', len) ->
  withHashPtrCopy h $ \hp -> tzHashAppend hp s' $ fromIntegral len

(+>) :: ByteString -> Hash -> Hash
(+>) s h = fst $ unsafePerformIO $ unsafeUseAsCStringLen s $ \(s', len) ->
  withHashPtrCopy h $ \hp -> tzHashPrepend hp s' $ fromIntegral len

parse :: String -> Maybe Hash
parse s = (\(h, r) -> h <$ r) $ unsafePerformIO $ withHashPtrNew $ \hp -> withCAStringLen s $ \(s', len) ->
  if len == tzHashLen then Just <$> tzHashUnserialize hp s' else return Nothing
