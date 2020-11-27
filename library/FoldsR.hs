module FoldsR
where

import FoldsR.Prelude
import Data.Fold.R
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text.Internal.Encoding.Utf16 as TextUtf16
import qualified Data.Text.Internal.Private as TextPrivate
import qualified Data.Text.Internal.Unsafe.Char as TextChar
import qualified Data.Text.Array as TextArray
import qualified Data.Text as Text


{-|
Same as 'many', but immediately folds the value.
-}
foldMany :: Alternative f => R a b -> f a -> f b
foldMany (R extract step init) fa =
  let
    loop =
      liftA2 step fa loop <|>
      pure init
    in fmap extract loop

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

{-|
Transformer of chars,
replaces all space-like chars with space,
all newline-like chars with @\\n@,
and trims their duplicate sequences to single-char.
Oh yeah, it also trims whitespace from beginning and end.
-}
trimmingWhitespace :: R Char o -> R Char o
trimmingWhitespace (R executeInner progressInner finalizeInner) =
  R execute progress finalize
  where
    execute finalize =
      executeInner (finalize False False False)
    progress char next notFirst spacePending newlinePending =
      if isSpace char
        then if char == '\n' || char == '\r'
          then next notFirst False True
          else next notFirst True newlinePending
        else
          let
            mapper =
              if notFirst
                then if newlinePending
                  then progressInner '\n'
                  else if spacePending
                    then progressInner ' '
                    else id
                else id
            in
              mapper $ progressInner char $ next True False False
    finalize notFirst spacePending newlinePending =
      finalizeInner

{-|
Create a text chunk reducer,
applying a char reducer to every char in it.
-}
foldingTextChars :: R Char o -> R Text o
foldingTextChars (R extractInner stepInner initInner) =
  R extractInner step initInner
  where
    step text acc =
      Text.foldr stepInner acc text
