\begin{code}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.Types.IsRegex where

import           Text.RE.Types.Match
import           Text.RE.Types.Matches
import           Text.RE.Types.REOptions
import           Text.RE.Types.Replace
import           Text.RE.Types.SearchReplace
\end{code}


\begin{code}
-- | the 'IsRegex' class allows tools to be written that will work with
-- regex back end text type supported by the back end
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
  -- | extract the text of the RE from the RE
  regexSource           :: re -> s

  makeRegex         = makeRegexWith         minBound
  makeSearchReplace = makeSearchReplaceWith minBound
\end{code}
