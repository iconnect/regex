{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Text.RE.PCRE.RE
  ( re
  , reMS
  , reMI
  , reBS
  , reBI
  , reMultilineSensitive
  , reMultilineInsensitive
  , reBlockSensitive
  , reBlockInsensitive
  , re_
  , cp
  , regexType
  , RE
  , reOptions
  , reSource
  , reCaptureNames
  , reRegex
  , Options
  , prelude
  , preludeEnv
  , preludeTestsFailing
  , preludeTable
  , preludeSummary
  , preludeSources
  , preludeSource
  , noPreludeOptions
  , defaultOptions
  , unpackSimpleRegexOptions
  , compileRegex
  ) where

import           Data.Bits
import           Data.Functor.Identity
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Prelude.Compat
import           Text.RE
import           Text.RE.Internal.NamedCaptures
import           Text.RE.Internal.PreludeMacros
import           Text.RE.Internal.QQ
import           Text.RE.TestBench
import           Text.Regex.PCRE


re
  , reMS
  , reMI
  , reBS
  , reBI
  , reMultilineSensitive
  , reMultilineInsensitive
  , reBlockSensitive
  , reBlockInsensitive
  , re_ :: QuasiQuoter

re                       = re' $ Just minBound
reMS                     = reMultilineSensitive
reMI                     = reMultilineInsensitive
reBS                     = reBlockSensitive
reBI                     = reBlockInsensitive
reMultilineSensitive     = re' $ Just  MultilineSensitive
reMultilineInsensitive   = re' $ Just  MultilineInsensitive
reBlockSensitive         = re' $ Just  BlockSensitive
reBlockInsensitive       = re' $ Just  BlockInsensitive
re_                      = re'   Nothing

regexType :: RegexType
regexType = PCRE

data RE =
  RE
    { _re_options :: !Options
    , _re_source  :: !String
    , _re_cnames  :: !CaptureNames
    , _re_regex   :: !Regex
    }

reOptions :: RE -> Options
reOptions = _re_options

reSource :: RE -> String
reSource = _re_source

reCaptureNames :: RE -> CaptureNames
reCaptureNames = _re_cnames

reRegex  :: RE -> Regex
reRegex = _re_regex

type Options = Options_ RE CompOption ExecOption

instance IsOption SimpleRegexOptions RE CompOption ExecOption where
  makeOptions    = unpackSimpleRegexOptions

instance IsOption Mode        RE CompOption ExecOption where
  makeOptions md = Options md prelude def_comp_option def_exec_option

instance IsOption (Macros RE) RE CompOption ExecOption where
  makeOptions ms = Options minBound ms def_comp_option def_exec_option

instance IsOption CompOption  RE CompOption ExecOption where
  makeOptions co = Options minBound prelude co def_exec_option

instance IsOption ExecOption  RE CompOption ExecOption where
  makeOptions eo = Options minBound prelude def_comp_option eo

instance IsOption Options     RE CompOption ExecOption where
  makeOptions    = id

instance IsOption ()          RE CompOption ExecOption where
  makeOptions _  = unpackSimpleRegexOptions minBound

def_comp_option :: CompOption
def_comp_option = _options_comp defaultOptions

def_exec_option :: ExecOption
def_exec_option = _options_exec defaultOptions

noPreludeOptions :: Options
noPreludeOptions = defaultOptions { _options_macs = emptyMacros }

defaultOptions :: Options
defaultOptions = makeOptions (minBound::SimpleRegexOptions)

unpackSimpleRegexOptions :: SimpleRegexOptions -> Options
unpackSimpleRegexOptions sro =
  Options
    { _options_mode = minBound
    , _options_macs = prelude
    , _options_comp = comp
    , _options_exec = defaultExecOpt
    }
  where
    comp =
      wiggle ml compMultiline $
      wiggle ci compCaseless
        defaultCompOpt

    wiggle True  m v = v .|.            m
    wiggle False m v = v .&. complement m

    (ml,ci) = case sro of
        MultilineSensitive    -> (,) True  False
        MultilineInsensitive  -> (,) True  True
        BlockSensitive        -> (,) False False
        BlockInsensitive      -> (,) False True

compileRegex :: ( IsOption o RE CompOption ExecOption
                , Functor m
                , Monad   m
                )
             => o
             -> String
             -> m RE
compileRegex = compileRegex_ . makeOptions

compileRegex_ :: ( Functor m , Monad m )
              => Options
              -> String
              -> m RE
compileRegex_ os re_s = uncurry mk <$> compileRegex' os re_s
  where
    mk cnms rex =
      RE
        { _re_options = os
        , _re_source  = re_s
        , _re_cnames  = cnms
        , _re_regex   = rex
        }

re' :: Maybe SimpleRegexOptions -> QuasiQuoter
re' mb = case mb of
  Nothing  ->
    (qq0 "re_")
      { quoteExp = parse minBound (\rs->[|flip unsafeCompileRegex rs|])
      }
  Just sro ->
    (qq0 "re")
      { quoteExp = parse sro (\rs->[|unsafeCompileRegexSimple sro rs|])
      }
  where
    parse :: SimpleRegexOptions -> (String->Q Exp) -> String -> Q Exp
    parse sro mk rs = either error (\_->mk rs) $ compileRegex_ os rs
      where
        os = unpackSimpleRegexOptions sro

unsafeCompileRegexSimple :: SimpleRegexOptions -> String -> RE
unsafeCompileRegexSimple sro re_s = unsafeCompileRegex os re_s
  where
    os = unpackSimpleRegexOptions sro

unsafeCompileRegex :: IsOption o RE CompOption ExecOption
                   => o
                   -> String
                   -> RE
unsafeCompileRegex = unsafeCompileRegex_ . makeOptions

unsafeCompileRegex_ :: Options -> String -> RE
unsafeCompileRegex_ os = either oops id . compileRegex os
  where
    oops = error . ("unsafeCompileRegex: " ++)

compileRegex' :: (Functor m,Monad m)
              => Options
              -> String
              -> m (CaptureNames,Regex)
compileRegex' Options{..} s0 = do
    (cnms,s2) <- either fail return $ extractNamedCaptures s1
    (,) cnms <$> makeRegexOptsM _options_comp _options_exec s2
  where
    s1 = expandMacros reSource _options_mode _options_macs s0

prelude :: Macros RE
prelude = runIdentity $ preludeMacros mk PCRE ExclCaptures
  where
    mk = Identity . unsafeCompileRegex_ noPreludeOptions

preludeTestsFailing :: [MacroID]
preludeTestsFailing = badMacros preludeEnv

preludeEnv :: MacroEnv
preludeEnv = preludeMacroEnv PCRE

preludeTable :: String
preludeTable = preludeMacroTable PCRE

preludeSummary :: PreludeMacro -> String
preludeSummary = preludeMacroSummary PCRE

preludeSources :: String
preludeSources = preludeMacroSources PCRE

preludeSource :: PreludeMacro -> String
preludeSource = preludeMacroSource PCRE
