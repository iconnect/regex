
module Text.RE.ZeInternals.Types.SearchReplace
  ( SearchReplace(..)
  ) where

-- | contains a compiled RE and replacement template
data SearchReplace re s =
  SearchReplace
    { getSearch   :: !re    -- ^ the RE
    , getTemplate :: !s     -- ^ the replacement template
    }
  deriving (Show)

instance Functor (SearchReplace re) where
  fmap f (SearchReplace re x) = SearchReplace re (f x)
