{-# LANGUAGE RecordWildCards            #-}

module Text.RE.SearchReplace
  (
  -- * Serach and Replace
    SearchReplace(..)
  , searchReplaceFirst
  , searchReplaceAll
  ) where

import           Text.RE.Types.IsRegex
import           Text.RE.Types.Replace
import           Text.RE.Types.SearchReplace


-- | searching and replacing the first occurrence
searchReplaceAll :: IsRegex re s => SearchReplace re s -> s -> s
searchReplaceAll SearchReplace{..} = replaceAll getTemplate . matchMany getSearch

-- | searching and replaceing all occurrences
searchReplaceFirst :: IsRegex re s => SearchReplace re s -> s -> s
searchReplaceFirst SearchReplace{..} = replace    getTemplate . matchOnce getSearch
