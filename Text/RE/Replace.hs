module Text.RE.Replace
  (
  -- * The Replacing Tutorial
  -- $tutorial

  -- * replaceAll
    replaceAll
  , replaceAllCaptures
  , replaceAllCaptures_
  , replaceAllCapturesM
  -- * replace
  , replace
  , replaceCaptures
  , replaceCaptures_
  , replaceCapturesM
  -- * REContext and RELocation
  , REContext(..)
  , RELocation(..)
  , isTopLocation
  -- * Matches
  , Matches(..)
  , anyMatches
  , countMatches
  , matches
  , mainCaptures
  -- * Match
  , Match(..)
  , noMatch
  , emptyMatchArray
  , matched
  , matchedText
  , matchCapture
  , matchCaptures
  , (!$$)
  , captureText
  , (!$$?)
  , captureTextMaybe
  , (!$)
  , capture
  , (!$?)
  , captureMaybe
  , convertMatchText
  -- * Capture
  , Capture(..)
  , hasCaptured
  , capturePrefix
  , captureSuffix
  -- * CaptureID
  , CaptureID(..)
  , CaptureNames
  , noCaptureNames
  , CaptureName(..)
  , CaptureOrdinal(..)
  , findCaptureID
  -- * Replace and ReplaceMethods
  , Replace(..)
  , ReplaceMethods(..)
  , replaceMethods
  ) where

import           Text.RE.ZeInternals.Replace
import           Text.RE.ZeInternals.Types.Capture
import           Text.RE.ZeInternals.Types.CaptureID
import           Text.RE.ZeInternals.Types.Match
import           Text.RE.ZeInternals.Types.Matches

-- $tutorial
-- This API module covers the specialised regex tools for doing general
-- editing on text, including the internal details of the 'Matches' and
-- 'Match' types and the associated functions for extracting captures
-- and applying functions to them to transform the subject text.
--
-- See the tutorials at http://re-tutorial-replacing.regex.uk
