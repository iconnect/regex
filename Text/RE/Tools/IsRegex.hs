module Text.RE.Tools.IsRegex
  (
  -- * IsRegex
  -- $tutorial
    IsRegex(..)
  , SearchReplace(..)
  , searchReplaceAll
  , searchReplaceFirst
  ) where

import Text.RE.ZeInternals.Types.IsRegex

-- $tutorial
-- The @IsRegex@ class abstracts over each regex back end and the
-- text types they support allowing general regex tools to constructed.
--
-- See the Regex Tools tutorial at http://re-tutorial-tools.regex.uk
