{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Text.RE.Tools
  (
  -- * The Tools Tutorial
  -- $tutorial

  -- * Sed
    sed
  , sed'
  -- * Grep
  , grep
  , Verbosity(..)
  , Line(..)
  , grepLines
  , grepFilter
  , GrepScript
  , grepWithScript
  , report
  , linesMatched
  -- * Lex
  , alex
  , alex'
  -- * Find
  , FindMethods(..)
  , findMatches_
  , findMatches_'
  -- * IsRegex
  , IsRegex(..)
  , SearchReplace(..)
  , searchReplaceAll
  , searchReplaceFirst
  -- * Edit
  , Edits(..)
  , Edit(..)
  , LineEdit(..)
  , applyEdits
  , applyEdit
  , applyLineEdit
  -- * LineNo
  , LineNo(..)
  , firstLine
  , getLineNo
  , lineNo
  -- * Replace
  , module Text.RE.Replace
  ) where

import           Text.RE.Replace
import           Text.RE.Tools.Edit
import           Text.RE.Tools.Find
import           Text.RE.Tools.Grep
import           Text.RE.Tools.Lex
import           Text.RE.Tools.Sed

-- $tutorial
-- This API module provides some familiar RE tools on top of the core
-- package functions and types.
--
-- See the Regex Tools tutorial at http://re-tutorial-tools.regex.uk
