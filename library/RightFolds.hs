module RightFolds
where

import RightFolds.Prelude
import Data.Fold.R
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text.Internal.Encoding.Utf16 as TextUtf16
import qualified Data.Text.Internal.Private as TextPrivate
import qualified Data.Text.Internal.Unsafe.Char as TextChar
import qualified Data.Text.Array as TextArray


charText :: R Char Text
charText =
  R execute progress finalize
  where
    execute finalize =
      finalize [] 0 
    progress char next !list !arraySize =
      let
        codepoint =
          ord char
        in if codepoint < 0x10000
          then next (fromIntegral codepoint : list) (succ arraySize)
          else let
            cpBasis =
              codepoint - 0x10000
            byte1 =
              fromIntegral ((cpBasis `shiftR` 10) + 0xd800)
            byte2 =
              fromIntegral ((cpBasis .&. 0x3ff) + 0xdc00)
            in next (byte2 : byte1 : list) (arraySize + 2)
    finalize revListOfBytes arraySize =
      TextPrivate.runText $ \terminate -> do
        array <- TextArray.new arraySize
        let
          loop !offset list =
            case list of
              byte : listTail ->
                TextArray.unsafeWrite array offset byte *>
                loop (pred offset) listTail
              [] ->
                return ()
          in loop (pred arraySize) revListOfBytes
        terminate array arraySize
