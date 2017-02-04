\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}

module Text.RE.Tools.Grep
  ( grep
  , grepLines
  , GrepScript
  , grepScript
  ) where

import qualified Data.ByteString.Lazy.Char8               as LBS
import           Prelude.Compat
import           Text.Printf
import           Text.RE.Capture
import           Text.RE.IsRegex
import           Text.RE.LineNo


data Line =
  Line
    { _ln_no      :: LineNo
    , _ln_matches :: Matches LBS.ByteString
    }
  deriving (Show)

grep :: IsRegex re LBS.ByteString => re -> FilePath -> IO ()
grep rex fp = grepLines rex fp >>= putStr . report

grepLines :: IsRegex re LBS.ByteString => re -> FilePath -> IO [Line]
grepLines rex fp =
    grepScript [(rex,mk)] . LBS.lines <$> LBS.readFile fp
  where
    mk i mtchs = Just $ Line i mtchs

type GrepScript re s t = [(re,LineNo -> Matches s -> Maybe t)]

grepScript :: IsRegex re s => GrepScript re s t -> [s] -> [t]
grepScript scr = loop firstLine
  where
    loop _ []       = []
    loop i (ln:lns) = seq i $ choose i ln lns scr

    choose i _  lns []             = loop (succ i) lns
    choose i ln lns ((rex,f):scr') = case f i $ matchMany rex ln of
      Nothing -> choose i ln lns scr'
      Just t  -> t : loop (succ i) lns

report :: [Line] -> String
report = unlines . map fmt . lines_matched
  where
    fmt Line{..} =
      printf "%05d %s" (getLineNo _ln_no) $
          LBS.unpack $ matchesSource _ln_matches

lines_matched :: [Line] -> [Line]
lines_matched = filter $ anyMatches . _ln_matches
\end{code}
