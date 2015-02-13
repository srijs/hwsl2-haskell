{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module TZHash (Hash, (<+), (+>)) where

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
withHashPtrNew f = mallocForeignPtrBytes tzHashSize >>= \fp -> withForeignPtr fp f >>= \r -> return (Hash fp, r)

withHashPtrNew_ f = withHashPtrNew f >>= return . fst

withHashPtrCopy :: Hash -> (Ptr () -> IO a) -> IO (Hash, a)
withHashPtrCopy h f = withHashPtr h $ \hp -> withHashPtrNew $ \hp' -> copyBytes hp' hp tzHashSize >> f hp'

withHashPtrCopy_ h f = withHashPtrCopy h f >>= return . fst

instance Show Hash where
  show h = unsafePerformIO $ withHashPtr h $ \hp -> mallocForeignPtrArray tzHashLen >>= \fp -> withForeignPtr fp $ \p' -> tzHashSerialize hp p' >> peekCStringLen (p', tzHashLen)

instance Eq Hash where
  a == b = unsafePerformIO $ withHashPtr a $ \ap -> withHashPtr b $ \bp -> tzHashEq ap bp >>= return . toBool

instance Monoid Hash where
  mempty = unsafePerformIO $ withHashPtrNew_ $ \p -> tzHashUnit p
  mappend a b = unsafePerformIO $ withHashPtr a $ \ap -> withHashPtr b $ \bp -> withHashPtrNew_ $ \hp -> tzHashConcat ap bp hp

(<+) :: Hash -> ByteString -> Hash
(<+) h s = unsafePerformIO $ unsafeUseAsCStringLen s $ \(s', len) ->
  withHashPtrCopy_ h $ \hp -> tzHashAppend hp s' (fromIntegral len)

(+>) :: ByteString -> Hash -> Hash
(+>) s h = unsafePerformIO $ unsafeUseAsCStringLen s $ \(s', len) ->
  withHashPtrCopy_ h $ \hp -> tzHashPrepend hp s' (fromIntegral len)

unserialize :: String -> Maybe Hash
unserialize s = unsafePerformIO $ withHashPtrNew (withCAStringLen s . unser) >>= \(h, r) -> return (r >> return h)
  where unser hp (s, 86) = tzHashUnserialize hp s >> return (Just ())
        unser _ _ = return Nothing
