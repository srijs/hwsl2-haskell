module Data.Hash.SL2.Unsafe (unsafeUseAsPtr, unsafeUseAsPtr2) where

import Foreign

import Data.Hash.SL2.Internal.Hash

unsafeUseAsPtr :: Hash -> (Ptr Hash -> IO a) -> IO a
unsafeUseAsPtr (H fp) f = withForeignPtr fp (f . castPtr)

unsafeUseAsPtr2 :: Hash -> Hash -> (Ptr Hash -> Ptr Hash -> IO a) -> IO a
unsafeUseAsPtr2 a b f = unsafeUseAsPtr a (unsafeUseAsPtr b . f)
