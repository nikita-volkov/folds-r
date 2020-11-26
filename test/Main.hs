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
import qualified Data.Attoparsec.Text as Atto


main =
  defaultMain $ testGroup "All" $ [
    testGroup "charText" [
      testProperty "Text roundtrip" $ \(list :: [Char]) ->
        Text.pack list === run list FoldsR.charText
      ]
    ,
    testGroup "trimmingWhitespace" [
      testProperty "1" $ \ (text :: Text) ->
        let
          words =
            Text.words text
          spacedInput =
            Text.map (\ c -> if isSpace c then ' ' else c) text
          newlinedInput =
            Text.map (\ c -> if isSpace c then '\n' else c) text
          process text =
            run (Text.unpack text) (FoldsR.trimmingWhitespace FoldsR.charText)
          in
            Text.unwords words === process spacedInput .&&.
            Text.intercalate "\n" words === process newlinedInput
      ,
      testProperty "2" $ \ (text :: Text) ->
        let
          isNewline c =
            c == '\n' || c == '\r'
          isSpaceButNotNewline c =
            isSpace c && not (isNewline c)
          normalize separator condition =
            Text.split condition >>>
            filter (not . Text.null) >>>
            Text.intercalate separator
          expected =
            text &
            Text.split isNewline &
            fmap Text.strip &
            filter (not . Text.null) &
            Text.intercalate "\n" &
            Text.split isSpaceButNotNewline &
            filter (not . Text.null) &
            Text.intercalate " "
          actual =
            run (Text.unpack text) (FoldsR.trimmingWhitespace FoldsR.charText)
          in
            expected === actual
      ]
    ,
    testGroup "foldMany" [
      testProperty "" $ \(input :: Text) ->
        let
          actual =
            Atto.parseOnly (FoldsR.foldMany FoldsR.charText (Atto.satisfy isAlphaNum)) input
          expected =
            Atto.parseOnly (Text.pack <$> many (Atto.satisfy isAlphaNum)) input
          in expected === actual
      ]
    ]
