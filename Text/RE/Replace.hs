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
  ) where

import           Text.RE.ZeInternals.Replace
import           Text.RE.ZeInternals.Types.Capture
import           Text.RE.ZeInternals.Types.CaptureID
import           Text.RE.ZeInternals.Types.Match
import           Text.RE.ZeInternals.Types.Matches
