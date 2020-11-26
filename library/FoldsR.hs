module FoldsR
where

import FoldsR.Prelude
import Data.Fold.R
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text.Internal.Encoding.Utf16 as TextUtf16
import qualified Data.Text.Internal.Private as TextPrivate
import qualified Data.Text.Internal.Unsafe.Char as TextChar
import qualified Data.Text.Array as TextArray


{-|
Fold constructing text from chars.
-}
charText :: R Char Text
charText =
  R execute progress finalize
  where
    execute finalize =
      finalize [] 0 
    progress char next !bytes !arraySize =
      let
        codepoint =
          ord char
        in if codepoint < 0x10000
          then next (fromIntegral codepoint : bytes) (succ arraySize)
          else let
            cpBasis =
              codepoint - 0x10000
            byte1 =
              fromIntegral ((cpBasis `shiftR` 10) + 0xd800)
            byte2 =
              fromIntegral ((cpBasis .&. 0x3ff) + 0xdc00)
            in next (byte2 : byte1 : bytes) (arraySize + 2)
    finalize revListOfBytes arraySize =
      TextPrivate.runText $ \terminate -> do
        array <- TextArray.new arraySize
        let
          loop !offset bytes =
            case bytes of
              byte : bytesTail ->
                TextArray.unsafeWrite array offset byte *>
                loop (pred offset) bytesTail
              [] ->
                return ()
          in loop (pred arraySize) revListOfBytes
        terminate array arraySize
