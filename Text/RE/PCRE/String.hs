{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.PCRE.String
  ( (*=~)
  , (?=~)
  , (=~)
  , (=~~)
  , module Text.RE
  , module Text.RE.PCRE.RE
  ) where


import           Text.Regex.Base
import           Text.RE
import           Text.RE.Internal.AddCaptureNames
import           Text.RE.PCRE.RE
import qualified Text.Regex.PCRE               as PCRE


-- | find all matches in text
(*=~) :: String
      -> RE
      -> Matches String
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ match (reRegex rex) bs

-- | find first matches in text
(?=~) :: String
      -> RE
      -> Match String
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ match (reRegex rex) bs

-- | regex-base polymorphic match operator
(=~) :: ( RegexContext PCRE.Regex String a
        , RegexMaker   PCRE.Regex PCRE.CompOption PCRE.ExecOption String
        )
     => String
     -> RE
     -> a
(=~) bs rex = match (reRegex rex) bs

-- | regex-base monadic, polymorphic match operator
(=~~) :: ( Monad m
         , RegexContext PCRE.Regex String a
         , RegexMaker   PCRE.Regex PCRE.CompOption PCRE.ExecOption String
         )
      => String
      -> RE
      -> m a
(=~~) bs rex = matchM (reRegex rex) bs

instance IsRegex RE String where
  matchOnce   = flip (?=~)
  matchMany   = flip (*=~)
  regexSource = reSource
