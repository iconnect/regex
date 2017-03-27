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

module Text.RE.TDFA.Sequence
  (
  -- * Tutorial
  -- $tutorial

  -- * The 'Matches' and 'Match' Operators
    (*=~)
  , (?=~)
  -- * The 'SearchReplace' Operators
  , (*=~/)
  , (?=~/)
  -- * The Classic rexex-base match Operators
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
  , escapeREString
  , module Text.RE.TDFA.RE
  , module Text.RE.Internal.SearchReplace.TDFA.Sequence
  ) where

import           Prelude.Compat
import qualified Data.Sequence                 as S
import           Data.Typeable
import           Text.Regex.Base
import           Text.RE
import           Text.RE.Internal.AddCaptureNames
import           Text.RE.Internal.SearchReplace.TDFA.Sequence
import           Text.RE.SearchReplace
import           Text.RE.Types.IsRegex
import           Text.RE.Types.REOptions
import           Text.RE.Types.Replace
import           Text.RE.TDFA.RE
import qualified Text.Regex.TDFA               as TDFA


-- | find all matches in text; e.g., to count the number of naturals in s:
--
--   @countMatches $ s *=~ [re|[0-9]+|]@
--
(*=~) :: (S.Seq Char)
      -> RE
      -> Matches (S.Seq Char)
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ match (reRegex rex) bs

-- | find first match in text
(?=~) :: (S.Seq Char)
      -> RE
      -> Match (S.Seq Char)
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ match (reRegex rex) bs

-- | search and replace all occurrences; e.g., this section will yield a function to
-- convert every a YYYY-MM-DD into a DD/MM/YYYY:
--
--   @(*=~/ [ed|${y}([0-9]{4})-0*${m}([0-9]{2})-0*${d}([0-9]{2})///${d}/${m}/${y}|])@
--
(*=~/) :: (S.Seq Char) -> SearchReplace RE (S.Seq Char) -> (S.Seq Char)
(*=~/) = flip searchReplaceAll

-- | search and replace the first occurrence only
(?=~/) :: (S.Seq Char) -> SearchReplace RE (S.Seq Char) -> (S.Seq Char)
(?=~/) = flip searchReplaceFirst

-- | the regex-base polymorphic match operator
(=~) :: ( Typeable a
        , RegexContext TDFA.Regex (S.Seq Char) a
        , RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption String
        )
     => (S.Seq Char)
     -> RE
     -> a
(=~) bs rex = addCaptureNames (reCaptureNames rex) $ match (reRegex rex) bs

-- | the regex-base monadic, polymorphic match operator
(=~~) :: ( Monad m
         , Functor m
         , Typeable a
         , RegexContext TDFA.Regex (S.Seq Char) a
         , RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption String
         )
      => (S.Seq Char)
      -> RE
      -> m a
(=~~) bs rex = addCaptureNames (reCaptureNames rex) <$> matchM (reRegex rex) bs

instance IsRegex RE (S.Seq Char) where
  matchOnce             = flip (?=~)
  matchMany             = flip (*=~)
  makeRegexWith         = \o -> compileRegexWith o . unpackR
  makeSearchReplaceWith = \o r t -> compileSearchReplaceWith o (unpackR r) (unpackR t)
  regexSource           = packR . reSource

-- $tutorial
-- We have a regex tutorial at <http://tutorial.regex.uk>.
