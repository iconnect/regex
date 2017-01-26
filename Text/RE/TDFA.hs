{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Text.RE.TDFA
  ( (*=~)
  , (?=~)
  , (=~)
  , (=~~)
  , module Text.RE
  , module Text.RE.TDFA.RE
  , module Text.RE.TDFA.ByteString
  , module Text.RE.TDFA.ByteString.Lazy
  , module Text.RE.TDFA.Sequence
  , module Text.RE.TDFA.String
  , module Text.RE.TDFA.Text
  , module Text.RE.TDFA.Text.Lazy
  ) where


import qualified Text.Regex.Base                          as B
import           Text.RE
import           Text.RE.Internal.AddCaptureNames
import           Text.RE.TDFA.RE
import qualified Text.Regex.TDFA                          as TDFA
import           Text.RE.TDFA.ByteString()
import           Text.RE.TDFA.ByteString.Lazy()
import           Text.RE.TDFA.Sequence()
import           Text.RE.TDFA.String()
import           Text.RE.TDFA.Text()
import           Text.RE.TDFA.Text.Lazy()


(*=~) :: IsRegex RE s
      => s
      -> RE
      -> Matches s
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ matchMany rex bs

(?=~) :: IsRegex RE s
      => s
      -> RE
      -> Match s
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ matchOnce rex bs

(=~) :: ( B.RegexContext TDFA.Regex s a
        , B.RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption s
        )
     => s
     -> RE
     -> a
(=~) bs rex = B.match (reRegex rex) bs

(=~~) :: ( Monad m
         , B.RegexContext TDFA.Regex s a
         , B.RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption s
         )
      => s
      -> RE
      -> m a
(=~~) bs rex = B.matchM (reRegex rex) bs
