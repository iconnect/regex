{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Text.RE.PCRE.ByteString
  ( (*=~)
  , (?=~)
  , (=~)
  , (=~~)
  , module Text.RE
  , module Text.RE.PCRE.RE
  ) where

import qualified Data.ByteString               as B
import           Text.Regex.Base
import           Text.RE
import           Text.RE.Internal.AddCaptureNames
import           Text.RE.PCRE.RE
import qualified Text.Regex.PCRE               as PCRE


-- | find all matches in text
(*=~) :: B.ByteString
      -> RE
      -> Matches B.ByteString
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ match (reRegex rex) bs

-- | find first matches in text
(?=~) :: B.ByteString
      -> RE
      -> Match B.ByteString
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ match (reRegex rex) bs

-- | regex-base polymorphic match operator
(=~) :: ( RegexContext PCRE.Regex B.ByteString a
        , RegexMaker   PCRE.Regex PCRE.CompOption PCRE.ExecOption String
        )
     => B.ByteString
     -> RE
     -> a
(=~) bs rex = match (reRegex rex) bs

-- | regex-base monadic, polymorphic match operator
(=~~) :: ( Monad m
         , RegexContext PCRE.Regex B.ByteString a
         , RegexMaker   PCRE.Regex PCRE.CompOption PCRE.ExecOption String
         )
      => B.ByteString
      -> RE
      -> m a
(=~~) bs rex = matchM (reRegex rex) bs

instance IsRegex RE B.ByteString where
  matchOnce   = flip (?=~)
  matchMany   = flip (*=~)
  regexSource = reSource
