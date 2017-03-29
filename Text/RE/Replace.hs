module Text.RE.Replace
  (
  -- * REContext and RELocation
    REContext(..)
  , RELocation(..)
  , isTopLocation
  -- * replaceAll
  , replaceAll
  , replaceAllCaptures
  , replaceAllCaptures_
  , replaceAllCapturesM
  -- * replace
  , replace
  , replaceCaptures
  , replaceCaptures_
  , replaceCapturesM
  -- * Replace and ReplaceMethods
  , Replace(..)
  , ReplaceMethods(..)
  , replaceMethods
  ) where

import           Text.RE.ZeInternals.Replace
