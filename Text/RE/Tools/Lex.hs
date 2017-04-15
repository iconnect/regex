module Text.RE.Tools.Lex
  (
  -- * Find
  -- $tutorial
    alex
  , alex'
  -- * IsRegex
  , IsRegex(..)
  , SearchReplace(..)
  , searchReplaceAll
  , searchReplaceFirst
  -- * Replace
  , module Text.RE.Replace
  ) where

import           Text.RE.Replace
import           Text.RE.Tools.IsRegex
import           Text.RE.ZeInternals.Tools.Lex

-- $tutorial
-- The Lex toolkit uses REs to identify tokens in a file, returning a
-- list of tokens.
--
-- See the Regex Tools tutorial at http://re-tutorial-tools.regex.uk
