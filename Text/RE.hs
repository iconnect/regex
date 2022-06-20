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

  -- * Further Use
  -- $further
  ) where

-- $tutorial
--
-- We have a regex tutorial at <http://tutorial.regex.uk>.

-- $use
--
-- This module just provides a brief overview of the regex package. You
-- will need to import one of the API modules of which there is a choice
-- which will depend upon two factors:
--
-- * Which flavour of regular expression do you want to use? If you need
--   Posix flavour REs then you will want the TDFA modules, otherwise its
--   PCRE for Perl-style REs.
--
-- * What type of text do you want to match: (slow) @String@s, @ByteString@,
--   @ByteString.Lazy@, @Text@, @Text.Lazy@ or the anachronistic @Seq Char@
--   or indeed some good old-fashioned polymorphic operators?
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

-- $further
-- For more specialist applications we have the following:
--
-- * "Text.RE.REOptions" for specifying back-end specific options;
-- * "Text.RE.Replace"   for the full text-replacement toolkit;
-- * "Text.RE.TestBench" for building up, testing and documenting;
--   macro environments  for use in REs;
-- * "Text.RE.Tools"     for an AWK-like text-processing toolkit.
