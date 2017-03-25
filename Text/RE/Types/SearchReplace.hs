{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}
#else
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Text.RE.Types.SearchReplace
  ( SearchReplace(..)
  ) where

import           Prelude.Compat


-- | contains a compiled RE and replacement template
data SearchReplace re s =
  SearchReplace
    { getSearch   :: !re    -- ^ the RE
    , getTemplate :: !s     -- ^ the replacement template
    }
  deriving (Show)

instance Functor (SearchReplace re) where
  fmap f (SearchReplace re x) = SearchReplace re (f x)
