{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE CPP                            #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Text.RE.TDFA
  (
  -- * Tutorial
  -- $tutorial

  -- * About this Module
  -- $about

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
  , escapeREString
  , module Text.RE.ZeInternals.TDFA
  -- * The [ed| ... |] quasi quoters
  , module Text.RE.ZeInternals.SearchReplace.TDFA
  -- * The Operator Instances
  -- $instances
  , module Text.RE.TDFA.ByteString
  , module Text.RE.TDFA.ByteString.Lazy
  , module Text.RE.TDFA.Sequence
  , module Text.RE.TDFA.String
  , module Text.RE.TDFA.Text
  , module Text.RE.TDFA.Text.Lazy
  ) where

import qualified Text.Regex.Base                          as B
import           Text.RE
import           Text.RE.ZeInternals.AddCaptureNames
import           Text.RE.ZeInternals.SearchReplace.TDFA
import           Text.RE.ZeInternals.TDFA
import qualified Text.Regex.TDFA                          as TDFA
import           Text.RE.TDFA.ByteString()
import           Text.RE.TDFA.ByteString.Lazy()
import           Text.RE.TDFA.Sequence()
import           Text.RE.TDFA.String()
import           Text.RE.TDFA.Text()
import           Text.RE.TDFA.Text.Lazy()
import           Text.RE.IsRegex
import           Text.RE.REOptions


-- | find all matches in text; e.g., to count the number of naturals in s:
--
--   @countMatches $ s *=~ [re|[0-9]+|]@
--
(*=~) :: IsRegex RE s
      => s
      -> RE
      -> Matches s
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ matchMany rex bs

-- | find first match in text
(?=~) :: IsRegex RE s
      => s
      -> RE
      -> Match s
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ matchOnce rex bs

-- | search and replace all occurrences; e.g., this section will yield a function to
-- convert every a YYYY-MM-DD into a DD/MM/YYYY:
--
--   @(*=~/ [ed|${y}([0-9]{4})-0*${m}([0-9]{2})-0*${d}([0-9]{2})///${d}/${m}/${y}|])@
--
(*=~/) :: IsRegex RE s => s -> SearchReplace RE s -> s
(*=~/) = flip searchReplaceAll

-- | search and replace the first occurrence only
(?=~/) :: IsRegex RE s => s -> SearchReplace RE s -> s
(?=~/) = flip searchReplaceFirst

-- | the regex-base polymorphic match operator
(=~) :: ( B.RegexContext TDFA.Regex s a
        , B.RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption s
        )
     => s
     -> RE
     -> a
(=~) bs rex = B.match (reRegex rex) bs

-- | the regex-base monadic, polymorphic match operator
(=~~) :: ( Monad m
         , B.RegexContext TDFA.Regex s a
         , B.RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption s
         )
      => s
      -> RE
      -> m a
(=~~) bs rex = B.matchM (reRegex rex) bs

-- $tutorial
-- We have a regex tutorial at <http://tutorial.regex.uk>.

-- $about
-- This module provides access to the back end through polymorphic functions
-- that operate over all of the String/Text/ByteString types supported by the
-- back end. If you don't need this generality you might want to consider
-- using one of the modules that have been specialised for each of these types:
--
-- * "Text.RE.TDFA.ByteString"
-- * "Text.RE.TDFA.ByteString.Lazy"
-- * "Text.RE.ZeInternals.TDFA"
-- * "Text.RE.TDFA.Sequence"
-- * "Text.RE.TDFA.String"
-- * "Text.RE.TDFA.Text"
-- * "Text.RE.TDFA.Text.Lazy"

-- $instances
--
-- These modules merely provide the 'IsRegex' instances.
