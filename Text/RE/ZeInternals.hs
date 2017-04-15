module Text.RE.ZeInternals
  (
  -- * The regex Internal Modules
  -- $internals

  -- * Text.RE.ZeInternals.AddCaptureNames
    addCaptureNames
  , addCaptureNamesToMatches
  , addCaptureNamesToMatch
  -- * Text.RE.ZeInternals.EscapeREString
  , escapeREString
  -- * Text.RE.ZeInternals.NamedCaptures
  , cp
  , extractNamedCaptures
  , idFormatTokenREOptions
  , Token
  , validToken
  , formatTokens
  , formatTokens'
  , formatTokens0
  , scan
  -- * Text.RE.ZeInternals.Replace
  , expandMacros
  -- * Text.RE.ZeInternals.PreludeMacros
  , PreludeMacro(..)
  , presentPreludeMacro
  , preludeMacros
  , preludeMacroTable
  , preludeMacroSummary
  , preludeMacroSources
  , preludeMacroSource
  , preludeMacroEnv
  -- * Text.RE.ZeInternals.SearchReplace
  , unsafeCompileSearchReplace_
  , compileSearchReplace_
  , compileSearchAndReplace_
  -- * Text.RE.ZeInternals.QQ
  , QQFailure(..)
  , qq0
  -- * Text.RE.ZeInternals.TestBench
  , mkTDFA
  , mkPCRE
  , badMacros
  ) where

import           Text.RE.ZeInternals.AddCaptureNames
import           Text.RE.ZeInternals.EscapeREString
import           Text.RE.ZeInternals.NamedCaptures
import           Text.RE.ZeInternals.PreludeMacros
import           Text.RE.ZeInternals.QQ
import           Text.RE.ZeInternals.Replace
import           Text.RE.ZeInternals.SearchReplace
import           Text.RE.ZeInternals.TestBench

-- $internals
-- This module contains just what the test suite (re-tests) in regex-examples
-- needs from the package internals to do its job and the ZeInternals
-- types and functions needed by the regex-with-pcre package
