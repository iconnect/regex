\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.Tools.Sed
  ( sed
  , sed'
  -- * IsRegex
  , IsRegex(..)
  -- * Edit
  , Edits(..)
  , Edit(..)
  , LineEdit(..)
  , applyEdits
  , applyEdit
  , applyLineEdit
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
import           Text.RE
import           Text.RE.Replace
import           Text.RE.Tools.Edit


-- | read a file, apply an 'Edits' script to each line it and
-- write the file out again; "-" is used to indicate standard input
-- standard output as appropriate
sed :: IsRegex re LBS.ByteString
    => Edits IO re LBS.ByteString
    -> FilePath
    -> FilePath
    -> IO ()
sed as i_fp o_fp = do
  lns  <- LBS.lines <$> read_file i_fp
  lns' <- sequence
    [ applyEdits lno as s
        | (lno,s)<-zip [firstLine..] lns
        ]
  write_file o_fp $ LBS.concat lns'

-- | apply an 'Edits' script to each line of the argument text
sed' :: (IsRegex re a,Monad m,Functor m)
     => Edits m re a
     -> a
     -> m a
sed' as t = do
  mconcat <$> sequence
    [ applyEdits lno as s
        | (lno,s)<-zip [firstLine..] $ linesR t
        ]

read_file :: FilePath -> IO LBS.ByteString
read_file "-" = LBS.getContents
read_file fp  = LBS.readFile fp

write_file :: FilePath -> LBS.ByteString ->IO ()
write_file "-" = LBS.putStr
write_file fp  = LBS.writeFile fp
\end{code}
