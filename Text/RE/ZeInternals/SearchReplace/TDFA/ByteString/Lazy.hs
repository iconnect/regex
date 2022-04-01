{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes      #-}
#else
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
#endif

module Text.RE.ZeInternals.SearchReplace.TDFA.ByteString.Lazy
  ( ed
  , edMultilineSensitive
  , edMultilineInsensitive
  , edBlockSensitive
  , edBlockInsensitive
  , edMS
  , edMI
  , edBS
  , edBI
  , ed_
  ) where

import qualified Data.ByteString.Lazy.Char8    as LBS
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.RE.REOptions
import           Text.RE.Tools.IsRegex
import           Text.RE.ZeInternals.SearchReplace.TDFAEdPrime
import           Text.RE.ZeInternals.TDFA

-- | @[ed| ... \/\/\/ ... |]@, is equivalent to @[edMultilineSensitive| ... \/\/\/ ... |]@,
-- compiling a case-sensitive, multi-line 'SearchReplace'
ed                      :: QuasiQuoter
ed                       = ed' sr_cast $ Just minBound

-- | @[edMultilineSensitive| ... \/\/\/ ... |]@ compiles a case-sensitive, multi-line 'SearchReplace' template
edMultilineSensitive    :: QuasiQuoter
edMultilineSensitive     = ed' sr_cast $ Just  MultilineSensitive

-- | @[edMultilineInsensitive| ... \/\/\/ ... |]@ compiles a case-insensitive, multi-line 'SearchReplace' template
edMultilineInsensitive  :: QuasiQuoter
edMultilineInsensitive   = ed' sr_cast $ Just  MultilineInsensitive

-- | @[edBlockSensitive| ... \/\/\/ ... |]@ compiles a case-sensitive, non-multi-line 'SearchReplace' template
edBlockSensitive        :: QuasiQuoter
edBlockSensitive         = ed' sr_cast $ Just  BlockSensitive

-- | @[edBlockInsensitive| ... \/\/\/ ... |]@ compiles a case-insensitive, non-multi-line 'SearchReplace' template
edBlockInsensitive      :: QuasiQuoter
edBlockInsensitive       = ed' sr_cast $ Just  BlockInsensitive

-- | @[edMS| ... \/\/\/ ... |]@ is a shorthand for @[edMultilineSensitive| ... \/\/\/ ... |]@
edMS                    :: QuasiQuoter
edMS                     = edMultilineSensitive

-- | @[edMI| ... \/\/\/ ... |]@ is a shorthand for @[edMultilineInsensitive| ... \/\/\/ ... |]@
edMI                    :: QuasiQuoter
edMI                     = edMultilineInsensitive

-- | @[edBS| ... \/\/\/ ... |]@ is a shorthand for @[edBlockSensitive| ... \/\/\/ ... |]@
edBS                    :: QuasiQuoter
edBS                     = edBlockSensitive

-- | @[edBI| ... \/\/\/ ... |]@ is a shorthand for @[edBlockInsensitive| ... \/\/\/ ... |]@
edBI                    :: QuasiQuoter
edBI                     = edBlockInsensitive

-- | @[ed_| ... \/\/\/ ... |]@ compiles a 'SearchReplace' template to produce a function that
-- takes the RE options (e.g., a 'SimpleREOptions' value) and yields the
-- 'SearchReplace' template compiled with those options. For example,
--
--   @s *=~/ [ed_|${hex}([0-9a-f]+)\/\/\/0x${hex}|] MultilineInsensitive@
--
-- prefixes the hexadecimal digit strings in s with @0x@, allowing for
-- upper- or lower-case hex digits (which is entirely equivalent
-- in this example to just using @[edMultilineInsensitive|[0-9a-f]+|]@).
ed_                     :: QuasiQuoter
ed_                      = ed' fn_cast   Nothing

sr_cast :: Q Exp
sr_cast = [|\x -> x :: SearchReplace RE LBS.ByteString|]

fn_cast :: Q Exp
fn_cast = [|\f x -> f x :: SearchReplace RE LBS.ByteString|]
