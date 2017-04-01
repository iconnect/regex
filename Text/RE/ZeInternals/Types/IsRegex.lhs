\begin{code}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.ZeInternals.Types.IsRegex
  ( IsRegex(..)
  , SearchReplace(..)
  , searchReplaceAll
  , searchReplaceFirst
  ) where

import           Text.RE.REOptions
import           Text.RE.Replace
import           Text.RE.ZeInternals.EscapeREString
import           Text.RE.ZeInternals.Types.SearchReplace
\end{code}

\begin{code}
-- | the 'IsRegex' class allows polymorhic tools to be written that
-- will work with a variety of regex back ends and text types
class Replace s => IsRegex re s where
  -- | finding the first match
  matchOnce             :: re -> s -> Match s
  -- | finding all matches
  matchMany             :: re -> s -> Matches s
  -- | compiling an RE, failing if the RE is not well formed
  makeRegex             :: (Functor m,Monad m) => s -> m re
  -- | comiling an RE, specifying the 'SimpleREOptions'
  makeRegexWith         :: (Functor m,Monad m) => SimpleREOptions -> s -> m re
  -- | compiling a 'SearchReplace' template from the RE text and the template Text, failing if they are not well formed
  makeSearchReplace     :: (Functor m,Monad m,IsRegex re s) => s -> s -> m (SearchReplace re s)
  -- | compiling a 'SearchReplace' template specifing the 'SimpleREOptions' for the RE
  makeSearchReplaceWith :: (Functor m,Monad m,IsRegex re s) => SimpleREOptions -> s -> s -> m (SearchReplace re s)
  -- | incorporate an escaped string into a compiled RE with the default options
  makeEscaped           :: (Functor m,Monad m) => (s->s) -> s -> m re
  -- | incorporate an escaped string into a compiled RE with the specified 'SimpleREOptions'
  makeEscapedWith       :: (Functor m,Monad m) => SimpleREOptions -> (s->s) -> s -> m re
  -- | extract the text of the RE from the RE
  regexSource           :: re -> s

  makeRegex           = makeRegexWith         minBound
  makeSearchReplace   = makeSearchReplaceWith minBound
  makeEscaped         = makeEscapedWith       minBound
  makeEscapedWith o f = makeRegexWith o . f . packR . escapeREString . unpackR

-- | search and replace all matches in the argument text; e.g., this function
-- will convert every YYYY-MM-DD format date in its argument text into a
-- DD\/MM\/YYYY date:
--
--   @searchReplaceAll [ed|${y}([0-9]{4})-0*${m}([0-9]{2})-0*${d}([0-9]{2})\/\/\/${d}\/${m}\/${y}|]@
--
searchReplaceAll :: IsRegex re s => SearchReplace re s -> s -> s
searchReplaceAll SearchReplace{..} = replaceAll getTemplate . matchMany getSearch

-- | search and replace the first occurrence only (if any) in the input text
-- e.g., to prefix the first string of four hex digits in the imput text,
-- if any, with @0x@:
--
--  @searchReplaceFirst [ed|[0-9A-Fa-f]{4}\/\/\/0x$0|]@
--
searchReplaceFirst :: IsRegex re s => SearchReplace re s -> s -> s
searchReplaceFirst SearchReplace{..} = replace    getTemplate . matchOnce getSearch
\end{code}
