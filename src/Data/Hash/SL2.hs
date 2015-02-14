-- |
-- Module     : Data.Hash.SL2
-- License    : MIT
-- Maintainer : Sam Rijs <srijs@airpost.net>
--
-- An algebraic hash function, inspired by the paper "Hashing with SL2" by
-- Tillich and Zemor.
--
-- The hash function is based on matrix multiplication in the special linear group
-- of degree 2, over a Galois field of order 2^127,  with all computations modulo
-- the polynomial x^127 + x^63 + 1.
--
-- This construction gives some nice properties, which traditional "bit-scambling"
-- hash functions don't possess, including it being composable. It holds:
--
-- prop> hash (m1 <> m2) == hash m1 <> hash m2
--
-- All operations in this package are implemented in a very efficient manner using SSE instructions.
--

module Data.Hash.SL2 (Hash, hash, (<+), (+>), (<|), (|>), parse) where

import Data.Hash.SL2.Internal
