module Text.RE.Internal.AddCaptureNames where

import           Text.RE


addCaptureNamesToMatches :: CaptureNames -> Matches a -> Matches a
addCaptureNamesToMatches cnms mtchs =
  mtchs { allMatches = map (addCaptureNamesToMatch cnms) $ allMatches mtchs }

addCaptureNamesToMatch :: CaptureNames -> Match a -> Match a
addCaptureNamesToMatch cnms mtch = mtch { captureNames = cnms }
