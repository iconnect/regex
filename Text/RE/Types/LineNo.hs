{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.RE.Types.LineNo where


newtype LineNo =
    ZeroBasedLineNo { getZeroBasedLineNo :: Int }
  deriving (Show,Enum)


firstLine :: LineNo
firstLine = ZeroBasedLineNo 0

getLineNo :: LineNo -> Int
getLineNo = (+1) . getZeroBasedLineNo

lineNo :: Int -> LineNo
lineNo = ZeroBasedLineNo . (\x->x-1)
