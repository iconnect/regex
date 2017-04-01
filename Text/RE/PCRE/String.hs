{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# OPTIONS_GHC -fno-warn-orphans           #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE CPP                            #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.PCRE.String
  (
  -- * Tutorial
  -- $tutorial

  -- * The 'Matches' and 'Match' Operators
    (*=~)
  , (?=~)
  -- * The 'SearchReplace' Operators
  , (*=~/)
  , (?=~/)
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
  -- * The Classic rexex-base Match Operators
  , (=~)
  , (=~~)
  -- * IsRegex
  , IsRegex(..)
  -- * The Quasi Quoters and Minor Functions
  -- $re
  , module Text.RE.ZeInternals.PCRE
  -- $ed
  , module Text.RE.ZeInternals.SearchReplace.PCRE.String
  ) where

import           Prelude.Compat

import           Data.Typeable
import           Text.RE.REOptions
import           Text.RE.Replace
import           Text.RE.ZeInternals.AddCaptureNames
import           Text.RE.ZeInternals.SearchReplace.PCRE.String
import           Text.RE.ZeInternals.PCRE
import           Text.RE.ZeInternals.Types.IsRegex
import           Text.Regex.Base
import qualified Text.Regex.PCRE               as PCRE


-- | find all the matches in the argument text; e.g., to count the number
-- of naturals in s:
--
--   @countMatches $ s *=~ [re|[0-9]+|]@
--
(*=~) :: String
      -> RE
      -> Matches String
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ match (reRegex rex) bs

-- | find the first match in the argument text; e.g., to test if there
-- is a natural number in the input text:
--
--   @matched $ s ?=~ [re|[0-9]+|]@
--
(?=~) :: String
      -> RE
      -> Match String
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ match (reRegex rex) bs

-- | search and replace all matches in the argument text; e.g., this section
-- will convert every YYYY-MM-DD format date in its argument text into a
-- DD\/MM\/YYYY date:
--
--   @(*=~\/ [ed|${y}([0-9]{4})-0*${m}([0-9]{2})-0*${d}([0-9]{2})\/\/\/${d}\/${m}\/${y}|])@
--
(*=~/) :: String -> SearchReplace RE String -> String
(*=~/) = flip searchReplaceAll

-- | search and replace the first occurrence only (if any) in the input text
-- e.g., to prefix the first string of four hex digits in the imput text,
-- if any, with @0x@:
--
--  @(?=~\/ [ed|[0-9A-Fa-f]{4}\/\/\/0x$0|])@
--
(?=~/) :: String -> SearchReplace RE String -> String
(?=~/) = flip searchReplaceFirst

-- | the regex-base polymorphic match operator
(=~) :: ( Typeable a
        , RegexContext PCRE.Regex String a
        , RegexMaker   PCRE.Regex PCRE.CompOption PCRE.ExecOption String
        )
     => String
     -> RE
     -> a
(=~) bs rex = addCaptureNames (reCaptureNames rex) $ match (reRegex rex) bs

-- | the regex-base monadic, polymorphic match operator
(=~~) :: ( Monad m
         , Functor m
         , Typeable a
         , RegexContext PCRE.Regex String a
         , RegexMaker   PCRE.Regex PCRE.CompOption PCRE.ExecOption String
         )
      => String
      -> RE
      -> m a
(=~~) bs rex = addCaptureNames (reCaptureNames rex) <$> matchM (reRegex rex) bs

instance IsRegex RE String where
  matchOnce             = flip (?=~)
  matchMany             = flip (*=~)
  makeRegexWith         = \o -> compileRegexWith o . unpackR
  makeSearchReplaceWith = \o r t -> compileSearchReplaceWith o (unpackR r) (unpackR t)
  regexSource           = packR . reSource

-- $tutorial
-- We have a regex tutorial at <http://tutorial.regex.uk>.

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
-- options dynamically, use the @[re_| ... |]@ and @[red_| ... \/\/\/ ... |]@
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
