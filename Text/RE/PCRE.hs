{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Text.RE.PCRE
  ( (*=~)
  , (?=~)
  , (=~)
  , (=~~)
  , module Text.RE
  , module Text.RE.PCRE.RE
  , module Text.RE.PCRE.ByteString
  , module Text.RE.PCRE.ByteString.Lazy
  , module Text.RE.PCRE.Sequence
  , module Text.RE.PCRE.String

  ) where


import qualified Text.Regex.Base                          as B
import           Text.RE
import           Text.RE.Internal.AddCaptureNames
import           Text.RE.PCRE.RE
import qualified Text.Regex.PCRE                          as PCRE
import           Text.RE.PCRE.ByteString()
import           Text.RE.PCRE.ByteString.Lazy()
import           Text.RE.PCRE.Sequence()
import           Text.RE.PCRE.String()


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

(=~) :: ( B.RegexContext PCRE.Regex s a
        , B.RegexMaker   PCRE.Regex PCRE.CompOption PCRE.ExecOption s
        )
     => s
     -> RE
     -> a
(=~) bs rex = B.match (reRegex rex) bs

(=~~) :: ( Monad m
         , B.RegexContext PCRE.Regex s a
         , B.RegexMaker   PCRE.Regex PCRE.CompOption PCRE.ExecOption s
         )
      => s
      -> RE
      -> m a
(=~~) bs rex = B.matchM (reRegex rex) bs
