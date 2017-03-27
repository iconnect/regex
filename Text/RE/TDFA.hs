{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE CPP                            #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Text.RE.TDFA
  (
  -- * Tutorial
  -- $tutorial
  --
  -- * The Match Operators
    (*=~)
  , (?=~)
  -- * The SearchReplace Operators
  , (*=~/)
  , (?=~/)
  -- * The Classic rexex-base Match Operators
  , (=~)
  , (=~~)
  -- * Matches
  , Matches
  , matchesSource
  , allMatches
  , anyMatches
  , countMatches
  , matches
  -- * Match
  , Match
  , matchSource
  , matched
  , matchedText
  -- * The 'RE' Type and Functions
  , RE
  , SimpleREOptions(..)
  , reSource
  , compileRegex
  , compileRegexWith
  , escape
  , escapeWith
  , module Text.RE.TDFA.RE
  -- * The [ed| ... |] quasi quoters
  , module Text.RE.Internal.SearchReplace.TDFA
  -- * The Operator Instances
  -- $instances
  , module Text.RE.TDFA.ByteString
  , module Text.RE.TDFA.ByteString.Lazy
  , module Text.RE.TDFA.Sequence
  , module Text.RE.TDFA.String
  , module Text.RE.TDFA.Text
  , module Text.RE.TDFA.Text.Lazy
  ) where

import qualified Text.Regex.Base                          as B
import           Text.RE
import           Text.RE.Internal.AddCaptureNames
import           Text.RE.Internal.SearchReplace.TDFA
import           Text.RE.TDFA.RE
import qualified Text.Regex.TDFA                          as TDFA
import           Text.RE.SearchReplace
import           Text.RE.TDFA.ByteString()
import           Text.RE.TDFA.ByteString.Lazy()
import           Text.RE.TDFA.Sequence()
import           Text.RE.TDFA.String()
import           Text.RE.TDFA.Text()
import           Text.RE.TDFA.Text.Lazy()
import           Text.RE.Types.IsRegex
import           Text.RE.Types.REOptions


-- | find all matches in text
(*=~) :: IsRegex RE s
      => s
      -> RE
      -> Matches s
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ matchMany rex bs

-- | find first match in text
(?=~) :: IsRegex RE s
      => s
      -> RE
      -> Match s
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ matchOnce rex bs

-- | search and replace once
(?=~/) :: IsRegex RE s => s -> SearchReplace RE s -> s
(?=~/) = flip searchReplaceFirst

-- | search and replace, all occurrences
(*=~/) :: IsRegex RE s => s -> SearchReplace RE s -> s
(*=~/) = flip searchReplaceAll

-- | the regex-base polymorphic match operator
(=~) :: ( B.RegexContext TDFA.Regex s a
        , B.RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption s
        )
     => s
     -> RE
     -> a
(=~) bs rex = B.match (reRegex rex) bs

-- | the regex-base monadic, polymorphic match operator
(=~~) :: ( Monad m
         , B.RegexContext TDFA.Regex s a
         , B.RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption s
         )
      => s
      -> RE
      -> m a
(=~~) bs rex = B.matchM (reRegex rex) bs

-- $tutorial
-- We have a regex tutorial at <http://tutorial.regex.uk>.

-- $instances
--
-- These modules merely provide the 'IsRegex' instances.
