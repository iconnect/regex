\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.Tools.Find
  (
  -- * Find
  -- $tutorial
    FindMethods(..)
  , findMatches_
  , findMatches_'
  -- * IsRegex
  , IsRegex(..)
  , SearchReplace(..)
  , searchReplaceAll
  , searchReplaceFirst
  -- * Replace
  , module Text.RE.Replace
  ) where

import qualified Data.List                      as L
import           Prelude.Compat
import           Text.RE.Replace
import           Text.RE.Tools.IsRegex
\end{code}


\begin{code}
-- | as we don't want the @directory@ and FilePath dependencies
-- we will abstract the three calls we need into this record type
data FindMethods s =
  FindMethods
    { doesDirectoryExistDM :: s -> IO Bool    -- ^ doesDirectoryExist from
                                              -- System.Directory
    , listDirectoryDM      :: s -> IO [s]     -- ^ either getDirectoryContents
                                              -- or listDirectory from
                                              -- System.Directory
    , combineDM            :: s -> s -> s     -- ^ </> from System.FilePath
    }
\end{code}


\begin{code}
-- | recursively list all files whose filename matches given RE,
-- sorting the list into ascending order; if the argument path has a
-- trailing '/' then it will be removed
findMatches_ :: IsRegex re s => FindMethods s -> re -> s -> IO [s]
findMatches_ fm = findMatches_' fm L.sort matched

-- | recursively list all files whose filename matches given RE,
-- using the given function to determine which matches to accept
findMatches_' :: IsRegex re s
              => FindMethods s         -- ^ the directory and filepath methods
              -> ([s]->[s])            -- ^ result post-processing function
              -> (Match s->Bool)       -- ^ filtering function
              -> re                    -- ^ re to be matched against the leaf filename
              -> s                     -- ^ root directory of the search
              -> IO [s]
findMatches_' fm srt tst re fp = srt <$> find_ fm tst re (packR "") fp

find_ :: IsRegex re s
      => FindMethods s
      -> (Match s->Bool)
      -> re
      -> s
      -> s
      -> IO [s]
find_ fm@FindMethods{..} tst re fn fp = do
  is_dir <- doesDirectoryExistDM fp
  case is_dir of
    True  -> do
      fns <- filter ordinary <$> listDirectoryDM fp
      concat <$>
        mapM (uncurry $ find_ fm tst re) [ (fn_,abs_path fn_) | fn_<-fns ]
    False -> return [ fp | lengthR fp /= 0 && tst (matchOnce re fn) ]
  where
    abs_path fn_ = fp `combineDM` fn_
    ordinary fn_ = not $ fn_ `elem` [packR ".",packR ".."]
\end{code}

\begin{code}
-- $tutorial
-- The Find toolkit traverses directory trees invoking actions for each
-- file that matches a RE.
--
-- See the Regex Tools tutorial at http://re-tutorial-tools.regex.uk
\end{code}
