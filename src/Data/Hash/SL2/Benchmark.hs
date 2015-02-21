import Data.Hash.SL2

import Data.Monoid
import Data.List (foldl')

import Control.Parallel.Strategies

import qualified Data.ByteString as B
import qualified Crypto.Hash.SHA1 as SHA1

import Criterion.Main

bs1M = B.pack $ take (1 * 1024 * 1024) $ cycle [0..255]
bs2M = B.pack $ take (2 * 1024 * 1024) $ cycle [0..255]
bs4M = B.pack $ take (4 * 1024 * 1024) $ cycle [0..255]

main = defaultMain
  [ bench "hwsl2 append" $ whnf (mempty <+) bs4M
  , bench "hwsl2 prepend" $ whnf (+> mempty) bs4M
  , bench "hwsl2 append parallel" $ whnf (mconcat . (parMap rpar hash)) [bs1M, bs1M, bs1M, bs1M]
  , bench "sha1" $ whnf SHA1.hash bs4M
  ]