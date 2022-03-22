\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}

module Text.RE.Tools.Grep
  (
  -- Grep
  -- $tutorial
    grep
  , Verbosity(..)
  , Line(..)
  , grepLines
  , grepFilter
  , GrepScript
  , grepWithScript
  , report
  , linesMatched
  -- * IsRegex
  , IsRegex(..)
  , SearchReplace(..)
  , searchReplaceAll
  , searchReplaceFirst
  -- * LineNo
  , LineNo(..)
  , firstLine
  , getLineNo
  , lineNo
  -- * Replace
  , module Text.RE.Replace
  ) where

import qualified Data.ByteString.Lazy.Char8               as LBS
import           Prelude.Compat
import           Text.Printf
import           Text.RE.Replace
import           Text.RE.Tools.IsRegex
import           Text.RE.ZeInternals.Types.LineNo
\end{code}


\begin{code}
-- | operates a bit like classic @grep@ printing out the lines matched
grep :: IsRegex re LBS.ByteString => Verbosity -> re -> FilePath -> IO ()
grep v rex fp = grepLines rex fp >>= putStr . report v
\end{code}

\begin{code}
-- | specifies whether to return the lines matched or missed
data Verbosity
  = LinesMatched
  | LinesNotMatched
  deriving (Show,Eq,Ord)
\end{code}

\begin{code}
-- | 'grepLines' returns a 'Line' for each line in the file, listing all
-- of the 'Matches' for that line
data Line s =
  Line
    { getLineNumber  :: LineNo    -- ^ the 'LineNo' for this line
    , getLineMatches :: Matches s -- ^ all the 'Matches' of the RE on this line
    }
  deriving (Show)
\end{code}

\begin{code}
-- | returns a 'Line' for each line in the file, enumerating all of the
-- matches for that line
grepLines :: IsRegex re LBS.ByteString
          => re
          -> FilePath
          -> IO [Line LBS.ByteString]
grepLines rex fp = grepFilter rex <$> LBS.readFile fp
\end{code}

\begin{code}
-- | returns a 'Line' for each line in the argument text, enumerating
-- all of the matches for that line
grepFilter :: IsRegex re s => re -> s -> [Line s]
grepFilter rex = grepWithScript [(rex,mk)] . linesR
  where
    mk i mtchs = Just $ Line i mtchs
\end{code}

\begin{code}
-- | a GrepScript lists RE-action associations, with the first RE to match
-- a line selecting the action to be executed on each line in the file
type GrepScript re s t = [(re,LineNo -> Matches s -> Maybe t)]

-- | given a list of lines, apply the 'GrepScript' to each line of the file
grepWithScript :: IsRegex re s => GrepScript re s t -> [s] -> [t]
grepWithScript scr = loop firstLine
  where
    loop _ []       = []
    loop i (ln:lns) = seq i $ choose i ln lns scr

    choose i _  lns []             = loop (succ i) lns
    choose i ln lns ((rex,f):scr') = case f i $ matchMany rex ln of
      Nothing -> choose i ln lns scr'
      Just t  -> t : loop (succ i) lns

-- | generate a grep report from a list of 'Line'
report :: Verbosity -> [Line LBS.ByteString] -> String
report v = unlines . map fmt . linesMatched v
  where
    fmt Line{..} =
      printf "%05d %s" (getLineNo getLineNumber) $
          LBS.unpack $ matchesSource getLineMatches

-- | given a 'velocity' flag filter out either the lines matched or not
-- matched
linesMatched :: Verbosity -> [Line s] -> [Line s]
linesMatched v = filter $ f . anyMatches . getLineMatches
  where
    f = case v of
      LinesMatched    -> id
      LinesNotMatched -> not
\end{code}

\begin{code}
-- $tutorial
-- The Grep toolkit matches REs against each line of a text.
--
-- See the Regex Tools tutorial at http://re-tutorial-tools.regex.uk
\end{code}
