{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE CPP                            #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Text.RE.PCRE
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
  , module Text.RE.ZeInternals.PCRE
  -- $ed
  , module Text.RE.ZeInternals.SearchReplace.PCRE
  -- * The Operator Instances
  -- $instances
  , module Text.RE.PCRE.ByteString
  , module Text.RE.PCRE.ByteString.Lazy
  , module Text.RE.PCRE.Sequence
  , module Text.RE.PCRE.String

  ) where


import qualified Text.Regex.Base                          as B
import           Text.RE
import           Text.RE.ZeInternals.AddCaptureNames
import           Text.RE.ZeInternals.SearchReplace.PCRE
import           Text.RE.ZeInternals.PCRE
import qualified Text.Regex.PCRE                          as PCRE
import           Text.RE.PCRE.ByteString()
import           Text.RE.PCRE.ByteString.Lazy()
import           Text.RE.PCRE.Sequence()
import           Text.RE.PCRE.String()
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

-- | search and replace the first occurrence only (if any) in the input text
-- e.g., to prefix the first string of four hex digits in the imput text,
-- if any, with @0x@:
--
--  @(?=~\/ [ed|[0-9A-Fa-f]{4}\/\/\/0x$0|])
--
(?=~/) :: IsRegex RE s => s -> SearchReplace RE s -> s
(?=~/) = flip searchReplaceFirst

-- | the regex-base polymorphic match operator
(=~) :: ( B.RegexContext PCRE.Regex s a
        , B.RegexMaker   PCRE.Regex PCRE.CompOption PCRE.ExecOption s
        )
     => s
     -> RE
     -> a
(=~) bs rex = B.match (reRegex rex) bs

-- | the regex-base monadic, polymorphic match operator
(=~~) :: ( Monad m
         , B.RegexContext PCRE.Regex s a
         , B.RegexMaker   PCRE.Regex PCRE.CompOption PCRE.ExecOption s
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
-- PCRE back end. If you don't need this generality you might find it easier
-- to work with one of the modules that have been specialised for each of these
-- types:
--
-- * "Text.RE.PCRE.ByteString"
-- * "Text.RE.PCRE.ByteString.Lazy"
-- * "Text.RE.ZeInternals.PCRE"
-- * "Text.RE.PCRE.Sequence"
-- * "Text.RE.PCRE.String"

-- $options
-- You can specify different compilation options by appending a
-- to the name of an [re| ... |] or [ed| ... \/\/\/ ... |] quasi quoter
-- to select the corresponding compilation option. For example, the
-- section,
--
--  @(?=~/ [edBlockInsensitive|foo$\/\/\/bar|])@
--
-- will replace a @foo@ suffix of the argument text, of any
-- capitalisation, with a (lower case) @bar@. If you need to specify the
-- options dynamically, use the @[re_| ... |]@ and @[red_| ... \/\/\/ ... |]@
-- quasi quoters, which generate functions that take an 'IsOption' option
-- (e.g., a 'SimpleReOptions' value) and yields a 'RE' or 'SearchReplace'
-- as apropriate. For example if you have a 'SimpleReOptions' value in
-- @sro@ then
--
--  @(?=~/ [ed_|foo$\/\/\/bar|] sro)@
--
-- will compile the @foo$@ RE according to the value of @sro@. For more
-- on specifying RE options see "Text.RE.REOptions".

-- $re
-- The @[re|.*|]@ quasi quoters, with variants for specifing different
-- options to the RE compiler (see "Text.RE.REOptions"), and the
-- specialised back-end types and functions.

-- $ed
-- The @[ed|.*\/\/\/foo|]@ quasi quoters, with variants for specifing different
-- options to the RE compiler (see "Text.RE.REOptions").

-- $instances
-- These modules merely provide the 'IsRegex' instances.
