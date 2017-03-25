{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.RE.Types.LineNo where


-- | our line numbers are of the proper zero-based kind
newtype LineNo =
    ZeroBasedLineNo { getZeroBasedLineNo :: Int }
  deriving (Show,Enum)

-- | the first line in a file
firstLine :: LineNo
firstLine = ZeroBasedLineNo 0

-- | extract a conventional 1-based line number
getLineNo :: LineNo -> Int
getLineNo = succ . getZeroBasedLineNo

-- | inject a conventional 1-based line number
lineNo :: Int -> LineNo
lineNo = ZeroBasedLineNo . pred
