{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# OPTIONS_GHC -fno-warn-orphans           #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE CPP                            #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
#endif

module Text.RE.PCRE.Sequence
  (
  -- * Tutorial
  -- $tutorial

  -- * The 'Matches' and 'Match' Operators
    (*=~)
  , (?=~)
  -- * The 'SearchReplace' Operators
  , (*=~/)
  , (?=~/)
  -- * The 'Matches' Type
  , Matches
  , matchesSource
  , allMatches
  , anyMatches
  , countMatches
  , matches
  -- * The 'Match' Type
  , Match
  , matchSource
  , matched
  , matchedText
  -- * The Macros and Parsers
  -- $macros
  , module Text.RE.TestBench.Parsers
  -- * The 'RE' Type
  , RE
  , reSource
  -- * Options
  -- $options
  , SimpleREOptions(..)
  -- * Compiling and Escaping REs
  , SearchReplace(..)
  , compileRegex
  , compileRegexWith
  , compileSearchReplace
  , compileSearchReplaceWith
  , escape
  , escapeWith
  , escapeREString
  -- * The Classic regex-base Match Operators
  , (=~)
  , (=~~)
  -- * The re Quasi Quoters
  -- $re
  , re
  , reMultilineSensitive
  , reMultilineInsensitive
  , reBlockSensitive
  , reBlockInsensitive
  , reMS
  , reMI
  , reBS
  , reBI
  , re_
  -- * The Ed Quasi Quoters
  -- $ed
  , edMultilineSensitive
  , edMultilineInsensitive
  , edBlockSensitive
  , edBlockInsensitive
  , ed
  , edMS
  , edMI
  , edBS
  , edBI
  , ed_
  -- * The cp Quasi Quoters
  , cp
  -- * IsRegex
  -- $isregex
  , module Text.RE.Tools.IsRegex
  ) where

import           Control.Monad.Fail
import qualified Data.Sequence                 as S
import           Data.Typeable
import           Prelude.Compat
import           Text.RE.REOptions
import           Text.RE.Replace
import           Text.RE.TestBench.Parsers
import           Text.RE.Tools.IsRegex
import           Text.RE.ZeInternals
import           Text.RE.ZeInternals.PCRE
import           Text.RE.ZeInternals.SearchReplace.PCRE.Sequence
import           Text.Regex.Base
import qualified Text.Regex.PCRE               as PCRE
-- NB regex-base instance imports maybe be needed for for some API modules

-- | find all the matches in the argument text; e.g., to count the number
-- of naturals in s:
--
--   @countMatches $ s *=~ [re|[0-9]+|]@
--
(*=~) :: (S.Seq Char)
      -> RE
      -> Matches (S.Seq Char)
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ match (reRegex rex) bs

-- | find the first match in the argument text; e.g., to test if there
-- is a natural number in the input text:
--
--   @matched $ s ?=~ [re|[0-9]+|]@
--
(?=~) :: (S.Seq Char)
      -> RE
      -> Match (S.Seq Char)
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ match (reRegex rex) bs

-- | search and replace all matches in the argument text; e.g., this section
-- will convert every YYYY-MM-DD format date in its argument text into a
-- DD\/MM\/YYYY date:
--
--   @(*=~\/ [ed|${y}([0-9]{4})-0*${m}([0-9]{2})-0*${d}([0-9]{2})\/\/\/${d}\/${m}\/${y}|])@
--
(*=~/) :: (S.Seq Char) -> SearchReplace RE (S.Seq Char) -> (S.Seq Char)
(*=~/) = flip searchReplaceAll

-- | search and replace the first occurrence only (if any) in the input text
-- e.g., to prefix the first string of four hex digits in the input text,
-- if any, with @0x@:
--
--  @(?=~\/ [ed|[0-9A-Fa-f]{4}\/\/\/0x$0|])@
--
(?=~/) :: (S.Seq Char) -> SearchReplace RE (S.Seq Char) -> (S.Seq Char)
(?=~/) = flip searchReplaceFirst

-- | the `regex-base` polymorphic match operator
(=~) :: ( Typeable a
        , RegexContext PCRE.Regex (S.Seq Char) a
        )
     => (S.Seq Char)
     -> RE
     -> a
(=~) bs rex = addCaptureNames (reCaptureNames rex) $ match (reRegex rex) bs

-- | the `regex-base` monadic, polymorphic match operator
(=~~) :: ( Monad m, MonadFail m
         , Functor m
         , Typeable a
         , RegexContext PCRE.Regex (S.Seq Char) a
         )
      => (S.Seq Char)
      -> RE
      -> m a
(=~~) bs rex = addCaptureNames (reCaptureNames rex) <$> matchM (reRegex rex) bs

instance IsRegex RE (S.Seq Char) where
  matchOnce             = flip (?=~)
  matchMany             = flip (*=~)
  makeRegexWith         = \o -> compileRegexWith o . unpackR
  makeSearchReplaceWith = \o r t -> compileSearchReplaceWith o (unpackR r) (unpackR t)
  regexSource           = packR . reSource

-- $tutorial
-- We have a regex tutorial at <http://tutorial.regex.uk>.

-- $macros
-- There are a number of RE macros and corresponding Haskell parsers
-- for parsing the matched text into appropriate Haskell types. See
-- the [Macros Tables](http://regex.uk/macros) for details.

-- $options
-- You can specify different compilation options by appending a
-- to the name of an [re| ... |] or [ed| ... \/\/\/ ... |] quasi quoter
-- to select the corresponding compilation option. For example, the
-- section,
--
--  @(?=~/ [edBlockInsensitive|foo$\/\/\/bar|])@
--
-- will replace a @foo@ suffix of the argument text, of any
-- capitalisation, with a (lower case) @bar@. If you need to specify the
-- options dynamically, use the @[re_| ... |]@ and @[ed_| ... \/\/\/ ... |]@
-- quasi quoters, which generate functions that take an 'IsOption' option
-- (e.g., a 'SimpleReOptions' value) and yields a 'RE' or 'SearchReplace'
-- as apropriate. For example if you have a 'SimpleReOptions' value in
-- @sro@ then
--
--  @(?=~/ [ed_|foo$\/\/\/bar|] sro)@
--
-- will compile the @foo$@ RE according to the value of @sro@. For more
-- on specifying RE options see "Text.RE.REOptions".

-- $re
-- The @[re|.*|]@ quasi quoters, with variants for specifing different
-- options to the RE compiler (see "Text.RE.REOptions"), and the
-- specialised back-end types and functions.

-- $ed
-- The @[ed|.*\/\/\/foo|]@ quasi quoters, with variants for specifing different
-- options to the RE compiler (see "Text.RE.REOptions").

-- $ed
-- The -- | the @[ed| ... \/\/\/ ... |]@ quasi quoters; for example,
--
--  @[ed|${y}([0-9]{4})-0*${m}([0-9]{2})-0*${d}([0-9]{2})\/\/\/${d}\/${m}\/${y}|])@
--
-- represents a @SearchReplace@ that will convert a YYYY-MM-DD format date
-- into a DD\/MM\/YYYY format date.
--
-- The only difference betweem these quasi quoters is the RE options that are set,
-- using the same conventions as the @[re| ... |]@ quasi quoters.

-- $isregex
-- The 'IsRegex' class is used to abstact over the different regex back ends and
-- the text types they work with -- see "Text.RE.Tools.IsRegex" for details.
