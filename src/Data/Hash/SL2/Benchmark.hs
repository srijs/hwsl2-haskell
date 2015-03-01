import Data.Hash.SL2

import Data.List (foldl')

import Control.Parallel.Strategies

import qualified Data.ByteString as B
import qualified Crypto.Hash.SHA256 as SHA256

import Criterion.Main

bs1M = B.pack $ take (1 * 1024 * 1024) $ cycle [0..255]
bs2M = B.pack $ take (2 * 1024 * 1024) $ cycle [0..255]
bs4M = B.pack $ take (4 * 1024 * 1024) $ cycle [0..255]

main = defaultMain
  [ bench "hwsl2 append" $ whnf (append unit) bs4M
  , bench "hwsl2 prepend" $ whnf (flip prepend unit) bs4M
  , bench "hwsl2 append parallel" $ whnf (concatAll . (parMap rpar hash)) [bs1M, bs1M, bs1M, bs1M]
  , bench "sha256" $ whnf SHA256.hash bs4M
  ]