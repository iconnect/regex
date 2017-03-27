{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes      #-}
#else
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
#endif

module Text.RE.Internal.SearchReplace.PCRE.ByteString.Lazy
  ( ed
  , edMS
  , edMI
  , edBS
  , edBI
  , edMultilineSensitive
  , edMultilineInsensitive
  , edBlockSensitive
  , edBlockInsensitive
  , ed_
  ) where

import qualified Data.ByteString.Lazy.Char8    as LBS
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.RE.PCRE.RE
import           Text.RE.Internal.SearchReplace.PCREEdPrime
import           Text.RE.SearchReplace
import           Text.RE.Types.REOptions


-- | the @[ed| ... /// ... |]@ quasi quoters
ed
  , edMS
  , edMI
  , edBS
  , edBI
  , edMultilineSensitive
  , edMultilineInsensitive
  , edBlockSensitive
  , edBlockInsensitive
  , ed_ :: QuasiQuoter

ed                       = ed' sr_cast $ Just minBound
edMS                     = edMultilineSensitive
edMI                     = edMultilineInsensitive
edBS                     = edBlockSensitive
edBI                     = edBlockInsensitive
edMultilineSensitive     = ed' sr_cast $ Just  MultilineSensitive
edMultilineInsensitive   = ed' sr_cast $ Just  MultilineInsensitive
edBlockSensitive         = ed' sr_cast $ Just  BlockSensitive
edBlockInsensitive       = ed' sr_cast $ Just  BlockInsensitive
ed_                      = ed' fn_cast   Nothing

sr_cast :: Q Exp
sr_cast = [|\x -> x :: SearchReplace RE LBS.ByteString|]

fn_cast :: Q Exp
fn_cast = [|\x -> x :: SimpleREOptions -> SearchReplace RE LBS.ByteString|]
