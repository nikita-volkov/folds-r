module Main where

import Prelude
import Data.Fold
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck
import qualified Data.Text as Text
import qualified FoldsR


main =
  defaultMain $ testGroup "All" $ [
    testGroup "charText" [
      testProperty "Text roundtrip" $ \(list :: [Char]) ->
        Text.pack list === run list FoldsR.charText
      ]
    ]
