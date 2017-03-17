module Text.RE.Tools
  (
  -- * The Tools
  -- $tools

  -- * Sed
    sed
  , sed'
  -- * Grep
  , grep
  , grepLines
  , GrepScript
  , grepScript
  , linesMatched
  -- * Lex
  , alex
  , alex'
  -- * IsRegex
  , IsRegex(..)
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
  -- * Text.RE
  , module Text.RE
  ) where

import           Text.RE
import           Text.RE.Tools.Edit
import           Text.RE.Tools.Grep
import           Text.RE.Tools.Lex
import           Text.RE.Tools.Sed
