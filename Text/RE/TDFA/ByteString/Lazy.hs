{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# OPTIONS_GHC -fno-warn-orphans           #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE CPP                            #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.TDFA.ByteString.Lazy
  (
  -- * Tutorial
  -- $tutorial
  --
  -- * The Match Operators
    (*=~)
  , (?=~)
  -- * The SearchReplace Operators
  , (*=~/)
  , (?=~/)
  -- * The Classic rexex-base Match Operators
  , (=~)
  , (=~~)
  -- * Matches
  , Matches
  , matchesSource
  , allMatches
  , anyMatches
  , countMatches
  , matches
  -- * Match
  , Match
  , matchSource
  , matched
  , matchedText
  -- * The 'RE' Type and Functions
  , RE
  , SimpleREOptions(..)
  , reSource
  , compileRegex
  , compileRegexWith
  , escape
  , escapeWith
  , module Text.RE.TDFA.RE
  ) where

import           Prelude.Compat
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Typeable
import           Text.Regex.Base
import           Text.RE
import           Text.RE.Internal.AddCaptureNames
import           Text.RE.SearchReplace
import           Text.RE.Types.IsRegex
import           Text.RE.Types.REOptions
import           Text.RE.Types.Replace
import           Text.RE.TDFA.RE
import qualified Text.Regex.TDFA               as TDFA


-- | find all matches in text
(*=~) :: LBS.ByteString
      -> RE
      -> Matches LBS.ByteString
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ match (reRegex rex) bs

-- | find first match in text
(?=~) :: LBS.ByteString
      -> RE
      -> Match LBS.ByteString
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ match (reRegex rex) bs

-- | search and replace once
(?=~/) :: LBS.ByteString -> SearchReplace RE LBS.ByteString -> LBS.ByteString
(?=~/) = flip searchReplaceFirst

-- | search and replace, all occurrences
(*=~/) :: LBS.ByteString -> SearchReplace RE LBS.ByteString -> LBS.ByteString
(*=~/) = flip searchReplaceAll

-- | the regex-base polymorphic match operator
(=~) :: ( Typeable a
        , RegexContext TDFA.Regex LBS.ByteString a
        , RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption String
        )
     => LBS.ByteString
     -> RE
     -> a
(=~) bs rex = addCaptureNames (reCaptureNames rex) $ match (reRegex rex) bs

-- | the regex-base monadic, polymorphic match operator
(=~~) :: ( Monad m
         , Functor m
         , Typeable a
         , RegexContext TDFA.Regex LBS.ByteString a
         , RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption String
         )
      => LBS.ByteString
      -> RE
      -> m a
(=~~) bs rex = addCaptureNames (reCaptureNames rex) <$> matchM (reRegex rex) bs

instance IsRegex RE LBS.ByteString where
  matchOnce             = flip (?=~)
  matchMany             = flip (*=~)
  makeRegexWith         = \o -> compileRegexWith o . unpackR
  makeSearchReplaceWith = \o r t -> compileSearchReplaceWith o (unpackR r) (unpackR t)
  regexSource           = packR . reSource

-- $tutorial
-- We have a regex tutorial at <http://tutorial.regex.uk>.
