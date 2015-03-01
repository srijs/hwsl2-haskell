{-# LANGUAGE Trustworthy #-}

module Data.Hash.SL2.Mutable
  ( valid
  , eq
  , unit
  , concat
  , append, prepend
  , foldAppend, foldPrepend
  , serialize, unserialize
  , withUnit, withCopy
  ) where

import Prelude hiding (concat)

import Foreign.Safe
import Foreign.C.String

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe

import Data.Foldable (Foldable, foldlM, foldrM)

import Data.Hash.SL2.Internal (Hash, hashLen)
import qualified Data.Hash.SL2.Internal as Internal
import Data.Hash.SL2.Unsafe

-- | /O(1)/ Check a hash for bit-level validity.
valid :: Ptr Hash -> IO Bool
{-# INLINE valid #-}
valid h = fmap toBool $ Internal.valid h

-- | /O(1)/ Compare the two hashes for equality.
eq :: Ptr Hash -> Ptr Hash -> IO Bool
{-# INLINE eq #-}
eq a b = fmap toBool $ Internal.eq a b

-- | /O(1)/ Set the 'Hash' to the empty value.
unit :: Ptr Hash -> IO ()
{-# INLINE unit #-}
unit h = Internal.unit h

-- | /O(1)/ Concatenate the second and third 'Hash', store the result in the first.
concat :: Ptr Hash -> Ptr Hash -> Ptr Hash -> IO ()
{-# INLINE concat #-}
concat c a b = Internal.concat c a b

-- | /O(n)/ Append the hash of the 'ByteString' to the existing 'Hash'.
append :: ByteString -> Ptr Hash -> IO ()
{-# INLINE append #-}
append s p = unsafeUseAsCStringLen s $ \(s', len) -> Internal.append p s' (fromIntegral len)

-- | /O(n)/ Prepend the hash of the 'ByteString' to the existing 'Hash'.
prepend :: ByteString -> Ptr Hash -> IO ()
{-# INLINE prepend #-}
prepend s p = unsafeUseAsCStringLen s $ \(s', len) -> Internal.prepend p s' (fromIntegral len)

-- | /O(n)/ Append the hash of every 'ByteString' to the existing 'Hash', from left to right.
foldAppend :: Foldable t => t ByteString -> Ptr Hash -> IO ()
{-# INLINE foldAppend #-}
foldAppend ss p = foldlM (const $ flip append p) () ss

-- | /O(n)/ Prepend the hash of every 'ByteString' to the existing 'Hash', from right to left.
foldPrepend :: Foldable t => t ByteString -> Ptr Hash -> IO ()
{-# INLINE foldPrepend #-}
foldPrepend ss p = foldrM (const . flip prepend p) () ss

-- | /O(1)/ Serialize the hash into a url-safe base64 representation.
serialize :: Ptr Hash -> IO String
{-# INLINE serialize #-}
serialize h = allocaBytes hashLen $ \p -> Internal.serialize h p >> peekCStringLen (p, hashLen)

-- | /O(1)/ Unserialize the hash from the representation generated by 'serialize'.
unserialize :: String -> Ptr Hash -> IO (Maybe ())
{-# INLINE unserialize #-}
unserialize s p = withCAStringLen s $ \(s', len) ->
  if len == hashLen then Just `fmap` Internal.unserialize p s' else return Nothing

withUnit :: (Ptr Hash -> IO a) -> IO (Hash, a)
{-# INLINE withUnit #-}
withUnit f = unsafeWithNew $ \p -> unit p >> f p

withCopy :: Hash -> (Ptr Hash -> IO a) -> IO (Hash, a)
{-# INLINE withCopy #-}
withCopy h f = unsafeWithNew $ \p -> poke p h >> f p
