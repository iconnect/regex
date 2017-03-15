\begin{code}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}

module Text.RE.Types.IsRegex where

import           Text.RE.Types.Match
import           Text.RE.Types.Matches
import           Text.RE.Types.Replace
\end{code}


\begin{code}
class Replace s => IsRegex re s where
  matchOnce   :: re -> s -> Match s
  matchMany   :: re -> s -> Matches s
  regexSource :: re -> String
\end{code}
