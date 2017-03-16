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
import           Text.RE.Types.Options
import           Text.RE.Types.Replace
\end{code}


\begin{code}
class Replace s => IsRegex re s where
  matchOnce     :: re -> s -> Match s
  matchMany     :: re -> s -> Matches s
  makeRegex     :: (Functor m,Monad m) => s -> m re
  makeRegexWith :: (Functor m,Monad m) => SimpleRegexOptions -> s -> m re
  regexSource   :: re -> s
\end{code}
