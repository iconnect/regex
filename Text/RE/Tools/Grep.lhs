\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}

module Text.RE.Tools.Grep
  ( grep
  , Line(..)
  , grepLines
  , GrepScript
  , grepScript
  , report
  , Verbosity(..)
  , linesMatched
  -- * IsRegex
  , IsRegex(..)
  -- * LineNo
  , LineNo(..)
  , firstLine
  , getLineNo
  , lineNo
  -- * Text.RE
  , module Text.RE
  ) where

import qualified Data.ByteString.Lazy.Char8               as LBS
import           Prelude.Compat
import           Text.Printf
import           Text.RE
import           Text.RE.ZeInternals.Types.LineNo


-- | operates a bit like classic @grep@ printing out the lines matched
grep :: IsRegex re LBS.ByteString => Verbosity -> re -> FilePath -> IO ()
grep v rex fp = grepLines rex fp >>= putStr . report v

-- | 'grepLines' returns a 'Line' for each line in the file, listing all
-- of the 'Matches' for that line
data Line =
  Line
    { getLineNumber  :: LineNo                  -- ^ the 'LineNo' for this line
    , getLineMatches :: Matches LBS.ByteString  -- ^ all the 'Matches' of the RE on this line
    }
  deriving (Show)

-- | returns a 'Line' for each line in the file enumerating all of the
-- matches for that line.
grepLines :: IsRegex re LBS.ByteString => re -> FilePath -> IO [Line]
grepLines rex fp =
    grepScript [(rex,mk)] . LBS.lines <$> LBS.readFile fp
  where
    mk i mtchs = Just $ Line i mtchs

-- | a GrepScript lists RE-action associations, with the first RE to match
-- a line selecting the action to be executed on each line in the file
type GrepScript re s t = [(re,LineNo -> Matches s -> Maybe t)]

-- | given a list of lines, apply the 'GrepScript' to each line of the file
grepScript :: IsRegex re s => GrepScript re s t -> [s] -> [t]
grepScript scr = loop firstLine
  where
    loop _ []       = []
    loop i (ln:lns) = seq i $ choose i ln lns scr

    choose i _  lns []             = loop (succ i) lns
    choose i ln lns ((rex,f):scr') = case f i $ matchMany rex ln of
      Nothing -> choose i ln lns scr'
      Just t  -> t : loop (succ i) lns

-- | generate a grep report from a list of 'Line'
report :: Verbosity -> [Line] -> String
report v = unlines . map fmt . linesMatched v
  where
    fmt Line{..} =
      printf "%05d %s" (getLineNo getLineNumber) $
          LBS.unpack $ matchesSource getLineMatches

-- | specifies whether to return the linss matched or missed
data Verbosity
  = LinesMatched
  | LinesNotMatched
  deriving (Show,Eq,Ord)

-- | given a 'velocity' flag filter out either the lines matched or not
-- matched
linesMatched :: Verbosity -> [Line] -> [Line]
linesMatched v = filter $ f . anyMatches . getLineMatches
  where
    f = case v of
      LinesMatched    -> id
      LinesNotMatched -> not
\end{code}
