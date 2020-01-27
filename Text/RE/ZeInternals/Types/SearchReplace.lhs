\begin{code}
module Text.RE.ZeInternals.Types.SearchReplace
  ( SearchReplace(..)
  ) where

\end{code}

\begin{code}
-- | contains a compiled RE and replacement template
data SearchReplace re s =
  SearchReplace
    { getSearch   :: !re    -- ^ the RE to match a string to replace
    , getTemplate :: !s     -- ^ the replacement template with ${cap}
                            -- used to identify a capture (by number or
                            -- name if one was given) and '$$' being
                            -- used to escape a single '$'
    }
  deriving (Show)
\end{code}

\begin{code}
instance Functor (SearchReplace re) where
  fmap f (SearchReplace re x) = SearchReplace re (f x)
\end{code}
