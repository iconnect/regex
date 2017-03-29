{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
-- |
-- Module      :  Text.RE
-- Copyright   :  (C) 2016-17 Chris Dornan
-- License     :  BSD3 (see the LICENSE file)
-- Maintainer  :  Chris Dornan <chris.dornan@irisconnect.com>
-- Stability   :  RFC
-- Portability :  portable

module Text.RE
  (
  -- * The Tutorial
  -- $tutorial

  -- * How to use this library
  -- $use

  -- * Matches
    Matches
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
  ) where

import           Text.RE.ZeInternals.Types.Match
import           Text.RE.ZeInternals.Types.Matches

-- $tutorial
--
-- We have a regex tutorial at <http://tutorial.regex.uk>.

-- $use
--
-- This module just provides an overview of the key type on which
-- the regex package is built. You will need to import one of the API
-- modules of which there is a choice which will depend upon two factors:
--
-- * Which flavour of regular expression do you want to use? If you want
--   Posix flavour REs then you want the TDFA modules, otherwise its
--   PCRE for Perl-style REs.
--
-- * What type of text do you want to match: (slow) @String@s, @ByteString@,
--   @ByteString.Lazy@, @Text@, @Text.Lazy@ or the anachronistic @Seq Char@
--   or indeed a good old-fashioned polymorphic operators?
--
-- While we aim to provide all combinations of these choices, some of them
-- are currently not available.  In the regex package we have:
--
-- * "Text.RE.TDFA.ByteString"
-- * "Text.RE.TDFA.ByteString.Lazy"
-- * "Text.RE.ZeInternals.TDFA"
-- * "Text.RE.TDFA.Sequence"
-- * "Text.RE.TDFA.String"
-- * "Text.RE.TDFA.Text"
-- * "Text.RE.TDFA.Text.Lazy"
-- * "Text.RE.TDFA"
--
-- The PCRE modules are contained in the separate @regex-with-pcre@
-- package:
--
-- * Text.RE.PCRE.ByteString
-- * Text.RE.PCRE.ByteString.Lazy
-- * Text.RE.ZeInternals.PCRE
-- * Text.RE.PCRE.Sequence
-- * Text.RE.PCRE.String
-- * Text.RE.PCRE
