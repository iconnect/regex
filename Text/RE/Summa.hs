module Text.RE.Summa
  ( -- $collection
    module Text.RE.IsRegex
  , module Text.RE.Replace
  , module Text.RE.TestBench
  , module Text.RE.Tools
  ) where

import Text.RE.IsRegex
import Text.RE.Replace
import Text.RE.TestBench
import Text.RE.Tools

-- $collection
--
-- This module collects together all of the generic regex APIs not
-- exported by the back-end-specific API modules. The regex
-- API is modular with only the most common types and functions being
-- exported by these modules but the remining modules may be imported
-- en masse by importing this module.
