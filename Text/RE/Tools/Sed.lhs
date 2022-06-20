\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.Tools.Sed
  (
  -- * Sed
  -- $tutorial
    sed
  , sed'
  -- * Edit
  , Edits(..)
  , Edit(..)
  , LineEdit(..)
  , applyEdits
  , applyEdit
  , applyLineEdit
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
import           Text.RE.Replace
import           Text.RE.Tools.Edit
\end{code}


\begin{code}
-- | read a file, apply an 'Edits' script to each line it and
-- write the file out again; "-" is used to indicate standard input
-- standard output as appropriate
sed :: IsRegex re LBS.ByteString
    => Edits IO re LBS.ByteString
    -> FilePath
    -> FilePath
    -> IO ()
sed escr i_fp o_fp = do
  lns  <- LBS.lines <$> read_file i_fp
  lns' <- sequence
    [ applyEdits lno escr s
        | (lno,s)<-zip [firstLine..] lns
        ]
  write_file o_fp $ LBS.concat lns'
\end{code}


\begin{code}
-- | apply an 'Edits' script to each line of the argument text
sed' :: (IsRegex re a,Monad m,Functor m)
     => Edits m re a
     -> a
     -> m a
sed' escr t = do
  mconcat <$> sequence
    [ applyEdits lno escr s
        | (lno,s)<-zip [firstLine..] $ linesR t
        ]
\end{code}


\begin{code}
read_file :: FilePath -> IO LBS.ByteString
read_file "-" = LBS.getContents
read_file fp  = LBS.readFile fp

write_file :: FilePath -> LBS.ByteString ->IO ()
write_file "-" = LBS.putStr
write_file fp  = LBS.writeFile fp
\end{code}


\begin{code}
-- $tutorial
-- The Sed toolkit applies @Edits@ scripts to each line
-- of a text, running the actions and adjusting each line
-- accordingly.
--
-- See the Regex Tools tutorial at http://re-tutorial-tools.regex.uk
\end{code}
