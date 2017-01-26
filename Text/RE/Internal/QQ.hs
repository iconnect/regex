module Text.RE.Internal.QQ where

import           Language.Haskell.TH.Quote


qq0 :: String -> QuasiQuoter
qq0 nm =
  QuasiQuoter
    { quoteExp  = const $ error $ oops "an expression"
    , quotePat  = const $ error $ oops "a pattern"
    , quoteType = const $ error $ oops "a type"
    , quoteDec  = const $ error $ oops "a declaration"
    }
  where
    oops sc = unwords
      [ "`"
      , nm
      , "` QuasiQuoter has been used in"
      , sc
      , "context but it should be used in an expresion context."
      ]
