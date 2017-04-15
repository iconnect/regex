{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.RE.ZeInternals.Types.CaptureID where

import qualified Data.HashMap.Strict            as HMS
import           Data.Hashable
import           Data.Ix
import qualified Data.Text                      as T


-- | CaptureID identifies captures, either by number
-- (e.g., [cp|1|]) or name (e.g., [cp|foo|]).
data CaptureID
  = IsCaptureOrdinal CaptureOrdinal   -- [cp|3|]
  | IsCaptureName    CaptureName      -- [cp|y|]
  deriving (Show,Ord,Eq)

-- | the dictionary for named captures stored in compiled regular
-- expressions associates
type CaptureNames = HMS.HashMap CaptureName CaptureOrdinal

-- | an empty 'CaptureNames' dictionary
noCaptureNames :: CaptureNames
noCaptureNames = HMS.empty

-- | a 'CaptureName' is just the text of the name
newtype CaptureName = CaptureName { getCaptureName :: T.Text }
  deriving (Show,Ord,Eq)

instance Hashable CaptureName where
  hashWithSalt i = hashWithSalt i . getCaptureName

-- | a 'CaptureOrdinal' is just the number of the capture, starting
-- with 0 for the whole of the text matched, then in leftmost,
-- outermost
newtype CaptureOrdinal = CaptureOrdinal { getCaptureOrdinal :: Int }
  deriving (Show,Ord,Eq,Enum,Ix,Num)

-- | look up a 'CaptureID' in the 'CaptureNames' dictionary
findCaptureID :: CaptureID -> CaptureNames -> Either String CaptureOrdinal
findCaptureID (IsCaptureOrdinal o) _   = Right o
findCaptureID (IsCaptureName    n) hms =
    maybe oops Right $ HMS.lookup n hms
  where
    oops = Left $ unlines $
      ("lookupCaptureID: " ++ T.unpack t ++ " not found in:") :
        [ "  "++T.unpack (getCaptureName nm) | nm <- HMS.keys hms ]
    t = getCaptureName n
