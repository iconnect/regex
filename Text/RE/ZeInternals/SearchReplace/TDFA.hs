{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes      #-}
#else
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
#endif

module Text.RE.ZeInternals.SearchReplace.TDFA
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

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Prelude.Compat
import           Text.RE.REOptions
import           Text.RE.ZeInternals.SearchReplace.TDFAEdPrime


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

ed                       = ed' cast $ Just minBound
edMS                     = edMultilineSensitive
edMI                     = edMultilineInsensitive
edBS                     = edBlockSensitive
edBI                     = edBlockInsensitive
edMultilineSensitive     = ed' cast $ Just  MultilineSensitive
edMultilineInsensitive   = ed' cast $ Just  MultilineInsensitive
edBlockSensitive         = ed' cast $ Just  BlockSensitive
edBlockInsensitive       = ed' cast $ Just  BlockInsensitive
ed_                      = ed' cast   Nothing

cast :: Q Exp
cast = [|id|]
