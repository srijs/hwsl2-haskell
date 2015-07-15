module Data.Hash.SL2.Chunk where

import Data.ByteString
import Data.Hash.SL2
import Data.Monoid

data Chunk = Chunk
  { getChunkHash :: Hash
  , getChunkBytes :: ByteString
  }

instance Eq Chunk where
  a == b = getChunkHash a == getChunkHash b

instance Ord Chunk where
  compare a b = compare (getChunkHash a) (getChunkHash b)

instance Monoid Chunk where
  mempty = Chunk mempty mempty
  mappend a b = Chunk (getChunkHash a <> getChunkHash b) (getChunkBytes a <> getChunkBytes b)
  mconcat as = Chunk (mconcat $ fmap getChunkHash as) (mconcat $ fmap getChunkBytes as)

fromByteString :: ByteString -> Chunk
fromByteString b = Chunk (hash b) b
