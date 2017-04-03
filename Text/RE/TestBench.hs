{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE OverloadedStrings                  #-}

module Text.RE.TestBench
  (
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
  -- ** Formatting Macros
  , testMacroEnv
  , runTests
  , runTests'
  -- * Parsing
  , parseInteger
  , parseHex
  , parseDouble
  , parseString
  , parseSimpleString
  , parseDate
  , parseSlashesDate
  , parseTimeOfDay
  , parseTimeZone
  , parseDateTime
  , parseDateTime8601
  , parseDateTimeCLF
  , parseShortMonth
  , shortMonthArray
  , IPV4Address
  , parseIPv4Address
  , Severity(..)
  , parseSeverity
  , severityKeywords
  -- * Text.RE
  , module Text.RE
  ) where

import           Text.RE
import           Text.RE.ZeInternals.TestBench
import           Text.RE.ZeInternals.TestBench.Parsers
