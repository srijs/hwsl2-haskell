import Data.Hash.SL2

import Data.Monoid

import qualified Data.ByteString as B
import qualified Crypto.Hash.SHA1 as SHA1

import Criterion.Main

bs1M = B.pack $ take (1 * 1024 * 1024) $ cycle [0..255]
bs2M = B.pack $ take (2 * 1024 * 1024) $ cycle [0..255]
bs4M = B.pack $ take (4 * 1024 * 1024) $ cycle [0..255]

main = defaultMain
  [ bench "hwsl2 append 1MB" $ whnf (mempty <+) bs1M
  , bench "hwsl2 prepend 1MB" $ whnf (+> mempty) bs1M
  , bench "sha1 1MB" $ whnf SHA1.hash bs1M
  ]
