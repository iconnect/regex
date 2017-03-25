{-# LANGUAGE DeriveDataTypeable         #-}

module Text.RE.Internal.QQ where

import           Control.Exception
import           Data.Typeable
import           Language.Haskell.TH.Quote


-- | used to throw an exception reporting an abuse of a quasi quoter
data QQFailure =
  QQFailure
    { _qqf_context   :: String  -- ^ in what context was the quasi quoter used
    , _qqf_component :: String  -- ^ how was the quasi quoter being abused
    }
  deriving (Show,Typeable)

instance Exception QQFailure where

-- | a quasi quoter that can be used in no context (to be extended with
-- the appropriate quasi quoter parser)
qq0 :: String -> QuasiQuoter
qq0 ctx =
  QuasiQuoter
    { quoteExp  = const $ throw $ QQFailure ctx "expression"
    , quotePat  = const $ throw $ QQFailure ctx "pattern"
    , quoteType = const $ throw $ QQFailure ctx "type"
    , quoteDec  = const $ throw $ QQFailure ctx "declaration"
    }
