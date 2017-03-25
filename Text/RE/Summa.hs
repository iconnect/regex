module Text.RE.Summa
  ( -- $collection
    module Text.RE.SearchReplace
  , module Text.RE.TestBench
  , module Text.RE.Tools
  , module Text.RE.Types
  ) where

import Text.RE.SearchReplace
import Text.RE.TestBench
import Text.RE.Tools
import Text.RE.Types

-- $collection
--
-- This module collects together all of the generic regex APIs not
-- exported by the API modules specific to each back end. The regex
-- API is modular with only the most common types and functions being
-- exported by these modules. The remining modules may be imported
-- individually or en masse by importing this module.
