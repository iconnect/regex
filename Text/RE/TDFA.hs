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
  -- * The 'RE' Type
  , RE
  , reSource
  -- * Options
  -- $options
  , SimpleREOptions(..)
  -- * Compiling and Escaping REs
  , compileRegex
  , compileRegexWith
  , escape
  , escapeWith
  , escapeREString
  -- * The Classic rexex-base Match Operators
  , (=~)
  , (=~~)
  -- * The Quasi Quoters and Minor Functions
  -- $re
  , module Text.RE.ZeInternals.TDFA
  -- $ed
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


-- | find all the matches in the argument text; e.g., to count the number
-- of naturals in s:
--
--   @countMatches $ s *=~ [re|[0-9]+|]@
--
(*=~) :: IsRegex RE s
      => s
      -> RE
      -> Matches s
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ matchMany rex bs

-- | find the first match in the argument text; e.g., to test if there
-- is a natural number in the input text:
--
--   @matched $ s ?=~ [re|[0-9]+|]@
--
(?=~) :: IsRegex RE s
      => s
      -> RE
      -> Match s
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ matchOnce rex bs

-- | search and replace all matches in the argument text; e.g., this section
-- will convert every YYYY-MM-DD format date in its argument text into a
-- DD\/MM\/YYYY date:
--
--   @(*=~\/ [ed|${y}([0-9]{4})-0*${m}([0-9]{2})-0*${d}([0-9]{2})\/\/\/${d}\/${m}\/${y}|])@
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
-- that operate over all of the String\/Text\/ByteString types supported by the
-- back end. If you don't need this generality then you might want to consider
-- using one of the modules that have been specialised for each of these types:
--
-- * "Text.RE.TDFA.ByteString"
-- * "Text.RE.TDFA.ByteString.Lazy"
-- * "Text.RE.ZeInternals.TDFA"
-- * "Text.RE.TDFA.Sequence"
-- * "Text.RE.TDFA.String"
-- * "Text.RE.TDFA.Text"
-- * "Text.RE.TDFA.Text.Lazy"

-- $re
-- The @[re|.*|]@ quasi quoters, with variants for specifing different
-- options to the RE compiler (see "Text.RE.REOptions"), and the
-- specialised back-end types and functions.

-- $ed
-- The @[ed|.*\/\/\/foo|]@ quasi quoters, with variants for specifing different
-- options to the RE compiler (see "Text.RE.REOptions").

-- $instances
--
-- These modules merely provide the 'IsRegex' instances.
