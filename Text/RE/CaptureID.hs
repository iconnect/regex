{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.RE.CaptureID where

import           Data.Ix
import           Data.Hashable
import qualified Data.HashMap.Strict            as HMS
import           Data.Maybe
import qualified Data.Text                      as T


data CaptureID
  = CID_ordinal CaptureOrdinal
  | CID_name    CaptureName
  deriving (Show,Ord,Eq)

type CaptureNames = HMS.HashMap CaptureName CaptureOrdinal

noCaptureNames :: CaptureNames
noCaptureNames = HMS.empty

newtype CaptureName = CaptureName { getCaptureName :: T.Text }
  deriving (Show,Ord,Eq)

instance Hashable CaptureName where
  hashWithSalt i = hashWithSalt i . getCaptureName

newtype CaptureOrdinal = CaptureOrdinal { getCaptureOrdinal :: Int }
  deriving (Show,Ord,Eq,Enum,Ix,Num)

findCaptureID :: CaptureID -> CaptureNames -> Int
findCaptureID (CID_ordinal o) _   = getCaptureOrdinal o
findCaptureID (CID_name    n) hms =
    getCaptureOrdinal $ fromMaybe oops $ HMS.lookup n hms
  where
    oops = error $ unlines $
      ("lookupCaptureID: " ++ T.unpack t ++ " not found in:") :
        [ "  "++T.unpack (getCaptureName nm) | nm <- HMS.keys hms ]
    t = getCaptureName n
