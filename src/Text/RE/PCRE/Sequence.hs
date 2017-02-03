{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.PCRE.Sequence
  ( (*=~)
  , (?=~)
  , (=~)
  , (=~~)
  , module Text.RE
  , module Text.RE.PCRE.RE
  ) where

import qualified Data.Sequence                 as S
import           Text.Regex.Base
import           Text.RE
import           Text.RE.Internal.AddCaptureNames
import           Text.RE.PCRE.RE
import qualified Text.Regex.PCRE               as PCRE


-- | find all matches in text
(*=~) :: (S.Seq Char)
      -> RE
      -> Matches (S.Seq Char)
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ match (reRegex rex) bs

-- | find first matches in text
(?=~) :: (S.Seq Char)
      -> RE
      -> Match (S.Seq Char)
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ match (reRegex rex) bs

-- | regex-base polymorphic match operator
(=~) :: ( RegexContext PCRE.Regex (S.Seq Char) a
        , RegexMaker   PCRE.Regex PCRE.CompOption PCRE.ExecOption String
        )
     => (S.Seq Char)
     -> RE
     -> a
(=~) bs rex = match (reRegex rex) bs

-- | regex-base monadic, polymorphic match operator
(=~~) :: ( Monad m
         , RegexContext PCRE.Regex (S.Seq Char) a
         , RegexMaker   PCRE.Regex PCRE.CompOption PCRE.ExecOption String
         )
      => (S.Seq Char)
      -> RE
      -> m a
(=~~) bs rex = matchM (reRegex rex) bs

instance IsRegex RE (S.Seq Char) where
  matchOnce   = flip (?=~)
  matchMany   = flip (*=~)
  regexSource = reSource
