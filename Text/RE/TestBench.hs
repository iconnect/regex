{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE OverloadedStrings                  #-}

module Text.RE.TestBench
  (
  -- * The Test Bench Tutorial
  -- $tutorial

  -- * The Test Bench
    MacroEnv
  , MacroDescriptor(..)
  , RegexSource(..)
  , WithCaptures(..)
  , RegexType
  , isTDFA
  , isPCRE
  , presentRegexType
  -- ** Constructing a MacrosEnv
  , mkMacros
  -- ** Formatting Macros
  , formatMacroTable
  , formatMacroSummary
  , formatMacroSources
  , formatMacroSource
  , mdRegexSource
  -- ** Formatting Macros
  , testMacroEnv
  , runTests
  , runTests'
  -- * The Parsers
  , module Text.RE.TestBench.Parsers
  -- * The Match Type
  , Match
  ) where

import           Text.RE.TestBench.Parsers
import           Text.RE.ZeInternals.TestBench
import           Text.RE.ZeInternals.Types.Match

-- $tutorial
-- This API module provides a test bench for developing, documenting and
-- testing regex RE macros.
--
-- See the tutorials at http://re-tutorial-testbench.regex.uk
