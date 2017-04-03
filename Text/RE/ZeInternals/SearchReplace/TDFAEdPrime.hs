{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}
#else
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
#endif

module Text.RE.ZeInternals.SearchReplace.TDFAEdPrime
  ( ed'
  ) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Prelude.Compat
import           Text.RE.REOptions
import           Text.RE.Replace
import           Text.RE.ZeInternals.QQ
import           Text.RE.ZeInternals.SearchReplace
import           Text.RE.ZeInternals.TDFA
import           Text.RE.ZeInternals.Types.IsRegex
import           Text.Regex.TDFA


-- | construct a quasi quoter from a casting function and @Just sro@
-- if the options are known, otherwise a function take takes the
-- 'SimpleREOptions' and constructs the 'SearchReplace' template
ed' :: Q Exp -> Maybe SimpleREOptions -> QuasiQuoter
ed' qe mb = case mb of
  Nothing  ->
    (qq0 "ed'")
      { quoteExp = parse minBound $ \rs -> AppE <$> qe <*> [|flip unsafe_compile_sr rs|]
      }
  Just sro ->
    (qq0 "ed'")
      { quoteExp = parse sro $ \rs -> AppE <$> qe <*> [|unsafe_compile_sr_simple sro rs|]
      }
  where
    parse :: SimpleREOptions -> (String->Q Exp) -> String -> Q Exp
    parse sro mk ts = either error (\_->mk ts) ei
      where
        ei :: Either String (SearchReplace RE String)
        ei = compileSearchReplace_ id (compileRegexWith sro) ts

unsafe_compile_sr_simple :: IsRegex RE s
                         => SimpleREOptions
                         -> String
                         -> SearchReplace RE s
unsafe_compile_sr_simple sro =
    unsafe_compile_sr $ unpackSimpleREOptions sro

unsafe_compile_sr :: ( IsOption o RE CompOption ExecOption
                              , IsRegex RE s
                              )
                           => o
                           -> String
                           -> SearchReplace RE s
unsafe_compile_sr os =
    unsafeCompileSearchReplace_ packR $ compileRegexWithOptions os
