module Data.Hash.SL2.Test where

import Data.Word

import Data.Hash.SL2
import Data.Hash.SL2.Internal (Hash)
import Data.Hash.SL2.Unsafe

import qualified Data.ByteString as B

import Foreign.Safe

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Property.Monoid

import Distribution.TestSuite.QuickCheck

import Data.Monoid

instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack arbitrary

instance Arbitrary Hash where
  arbitrary = fmap unsafePack $ mapM choose (take 64 $ cycle $ replicate 15 (0, 255) ++ [(0, 127)])

tests :: IO [Test]
tests = return

  [ testGroup "composition"

    [ testProperty "forms a monoid" $
        eq $ prop_Monoid (T :: T Hash)

    , testProperty "is distributive" $
        \a b -> hash (a <> b) == hash a <> hash b

    ]

  , testGroup "append"

    [ testGroup "single string" $

      [ testProperty "equal to ((. hash) . (<>))" $
          \a b -> ((. hash) . (<>)) a b == a <+ b
      ]

    , testGroup "multiple strings" $

      [ testProperty "equal to (foldl (<+))" $
          \a b -> (foldl (<+)) a b == a <| b
      ]

    ]

  , testGroup "prepend"

    [ testGroup "single string" $

      [ testProperty "equal to ((<>) . hash)" $
          \a b -> ((<>) . hash) a b == a +> b
      ]

    , testGroup "multiple strings" $

      [ testProperty "equal to (flip (foldr (+>)))" $
          \a b -> (flip (foldr (+>))) a b == a |> b
      ]

    ]

  ]
