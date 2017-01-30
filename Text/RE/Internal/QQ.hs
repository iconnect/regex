{-# LANGUAGE DeriveDataTypeable         #-}

module Text.RE.Internal.QQ where

import           Control.Exception
import           Data.Typeable
import           Language.Haskell.TH.Quote


data QQFailure =
  QQFailure
    { _qqf_context   :: String
    , _qqf_component :: String
    }
  deriving (Show,Typeable)

instance Exception QQFailure where

qq0 :: String -> QuasiQuoter
qq0 ctx =
  QuasiQuoter
    { quoteExp  = const $ throw $ QQFailure ctx "expression"
    , quotePat  = const $ throw $ QQFailure ctx "pattern"
    , quoteType = const $ throw $ QQFailure ctx "type"
    , quoteDec  = const $ throw $ QQFailure ctx "declaration"
    }
