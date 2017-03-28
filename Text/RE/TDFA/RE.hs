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

module Text.RE.TDFA.RE
  ( -- * About
    -- $about

    -- * RE Type
    RE
  , regexType
  , reOptions
  , reSource
  , reCaptureNames
  , reRegex
  -- * REOptions Type
  , REOptions
  , defaultREOptions
  , noPreludeREOptions
  -- * Compiling Regular Expressions
  , compileRegex
  , compileRegexWith
  , compileRegexWithOptions
  -- * Compiling Search-Replace Templates
  , compileSearchReplace
  , compileSearchReplaceWith
  , compileSearchReplaceWithREOptions
  -- * Escaping String
  , escape
  , escapeWith
  , escapeWithOptions
  , escapeREString
  -- * Macros Standard Environment
  , prelude
  , preludeEnv
  , preludeTestsFailing
  , preludeTable
  , preludeSummary
  , preludeSources
  , preludeSource
  , unpackSimpleREOptions
  -- * The Quasi Quoters
  , re
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
  ) where

import           Data.Functor.Identity
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Prelude.Compat
import           Text.RE.Internal.EscapeREString
import           Text.RE.Internal.NamedCaptures
import           Text.RE.Internal.PreludeMacros
import           Text.RE.Internal.QQ
import           Text.RE.Internal.SearchReplace
import           Text.RE.SearchReplace
import           Text.RE.TestBench
import           Text.RE.Types.CaptureID
import           Text.RE.Types.IsRegex
import           Text.RE.Types.REOptions
import           Text.RE.Types.Replace
import           Text.Regex.TDFA


-- | the RE type for this back end representing a well-formed, compiled
-- RE
data RE =
  RE
    { _re_options :: !REOptions
    , _re_source  :: !String
    , _re_cnames  :: !CaptureNames
    , _re_regex   :: !Regex
    }

-- | some functions in the "Text.RE.TestBench" need the back end to
-- be passed dynamically as a 'RegexType' parameters: use 'regexType'
-- fpr this backend
regexType :: RegexType
regexType =
  mkTDFA $ \txt env md -> txt =~ mdRegexSource regexType ExclCaptures env md

-- | extract the 'REOptions' from the @RE@
reOptions :: RE -> REOptions
reOptions = _re_options

-- | extract the RE source string from the @RE@
reSource :: RE -> String
reSource = _re_source

-- | extract the 'CaptureNames' from the @RE@
reCaptureNames :: RE -> CaptureNames
reCaptureNames = _re_cnames

-- | extract the back end compiled 'Regex' type from the @RE@
reRegex :: RE -> Regex
reRegex = _re_regex


------------------------------------------------------------------------
-- REOptions
------------------------------------------------------------------------

-- | and the REOptions for this back end (see "Text.RE.Types.REOptions"
-- for details)
type REOptions = REOptions_ RE CompOption ExecOption

instance IsOption SimpleREOptions RE CompOption ExecOption where
  makeREOptions    = unpackSimpleREOptions

instance IsOption (Macros RE) RE CompOption ExecOption where
  makeREOptions ms = REOptions ms def_comp_option def_exec_option

instance IsOption CompOption  RE CompOption ExecOption where
  makeREOptions co = REOptions prelude co def_exec_option

instance IsOption ExecOption  RE CompOption ExecOption where
  makeREOptions eo = REOptions prelude def_comp_option eo

instance IsOption REOptions     RE CompOption ExecOption where
  makeREOptions    = id

instance IsOption ()          RE CompOption ExecOption where
  makeREOptions _  = unpackSimpleREOptions minBound

-- | the default 'REOptions'
defaultREOptions :: REOptions
defaultREOptions = makeREOptions (minBound::SimpleREOptions)

-- | the default 'REOptions' but with no RE macros defined
noPreludeREOptions :: REOptions
noPreludeREOptions = defaultREOptions { optionsMacs = emptyMacros }

-- | convert a universal 'SimpleReOptions' into the 'REOptions' used
-- by this back end
unpackSimpleREOptions :: SimpleREOptions -> REOptions
unpackSimpleREOptions sro =
  REOptions
    { optionsMacs = prelude
    , optionsComp = comp
    , optionsExec = defaultExecOpt
    }
  where
    comp = defaultCompOpt
      { caseSensitive = cs
      , multiline     = ml
      }

    (ml,cs) = case sro of
        MultilineSensitive    -> (,) True  True
        MultilineInsensitive  -> (,) True  False
        BlockSensitive        -> (,) False True
        BlockInsensitive      -> (,) False False


------------------------------------------------------------------------
-- Compiling Regular Expressions
------------------------------------------------------------------------

-- | compile a 'String' into a 'RE' with the default options,
-- generating an error if the RE is not well formed
compileRegex :: (Functor m,Monad m) => String -> m RE
compileRegex = compileRegexWithOptions ()

-- | compile a 'String' into a 'RE' using the given @SimpleREOptions@,
-- generating an error if the RE is not well formed
compileRegexWith :: (Functor m,Monad m) => SimpleREOptions -> String -> m RE
compileRegexWith = compileRegexWithOptions

-- | compile a 'String' into a 'RE' using the given @SimpleREOptions@,
-- generating an error if the RE is not well formed
compileRegexWithOptions :: ( IsOption o RE CompOption ExecOption
                           , Functor m
                           , Monad   m
                           )
                        => o
                        -> String
                        -> m RE
compileRegexWithOptions = compileRegex_ . makeREOptions


------------------------------------------------------------------------
-- Compiling Search Replace Templates
------------------------------------------------------------------------

-- | compile a SearchReplace template generating errors if the RE or
-- the template are not well formed -- all capture references being checked
compileSearchReplace :: (Monad m,Functor m,IsRegex RE s)
                     => String
                     -> String
                     -> m (SearchReplace RE s)
compileSearchReplace = compileSearchReplaceWith minBound

-- | compile a SearchReplace template, with simple options, generating
-- errors if the RE or the template are not well formed -- all capture
-- references being checked
compileSearchReplaceWith :: (Monad m,Functor m,IsRegex RE s)
                         => SimpleREOptions
                         -> String
                         -> String
                         -> m (SearchReplace RE s)
compileSearchReplaceWith sro = compileSearchAndReplace_ packR $ compileRegexWith sro

-- | compile a SearchReplace template, with general options, generating
-- errors if the RE or the template are not well formed -- all capture
-- references being checked
compileSearchReplaceWithREOptions :: (Monad m,Functor m,IsRegex RE s)
                                  => REOptions
                                  -> String
                                  -> String
                                  -> m (SearchReplace RE s)
compileSearchReplaceWithREOptions os = compileSearchAndReplace_ packR $ compileRegexWithOptions os


------------------------------------------------------------------------
-- Escaping Strings
------------------------------------------------------------------------

-- | convert a string into a RE that matches that string, and apply it
-- to an argument continuation function to make up the RE string to be
-- compiled
escape :: (Functor m,Monad m)
       => (String->String)
       -> String
       -> m RE
escape = escapeWith minBound

-- | convert a string into a RE that matches that string, and apply it
-- to an argument continuation function to make up the RE string to be
-- compiled with the default options
escapeWith :: (Functor m,Monad m)
           => SimpleREOptions
           -> (String->String)
           -> String
           -> m RE
escapeWith = escapeWithOptions

-- | convert a string into a RE that matches that string, and apply it
-- to an argument continuation function to make up the RE string to be
-- compiled the given options
escapeWithOptions :: ( IsOption o RE CompOption ExecOption
                     , Functor m
                     , Monad m
                     )
                  => o
                  -> (String->String)
                  -> String
                  -> m RE
escapeWithOptions o f = compileRegexWithOptions o . f . escapeREString


------------------------------------------------------------------------
-- Macro Standard Environment
------------------------------------------------------------------------

-- | the standard table of 'Macros' used to compile REs (which can be
-- extended or replace: see "Text.RE.TestBench")
prelude :: Macros RE
prelude = runIdentity $ preludeMacros mk regexType ExclCaptures
  where
    mk = Identity . unsafeCompileRegex_ noPreludeREOptions

-- | the standard 'MacroEnv' for this back end (see "Text.RE.TestBench")
preludeEnv :: MacroEnv
preludeEnv = preludeMacroEnv regexType

-- | the macros in the standard environment that are failing their tests
-- (checked by the test suite to be empty)
preludeTestsFailing :: [MacroID]
preludeTestsFailing = badMacros $ preludeMacroEnv regexType

-- | a table the standard macros in markdown format
preludeTable :: String
preludeTable = preludeMacroTable regexType

-- | a summary of the macros in the standard environment for this back
-- end in plain text
preludeSummary :: PreludeMacro -> String
preludeSummary = preludeMacroSummary regexType

-- | a listing of the RE text for each macro in the standard environment
-- with all macros expanded to normal form
preludeSources :: String
preludeSources = preludeMacroSources regexType

-- | the prolude source of a given macro in the standard environment
preludeSource :: PreludeMacro -> String
preludeSource = preludeMacroSource regexType


------------------------------------------------------------------------
-- Quasi Quoters
------------------------------------------------------------------------

-- | the @[re| ... |]@ and @[ed| ... /// ... |]@ quasi quoters
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
  -- , ed
  -- , edMS
  -- , edMI
  -- , edBS
  -- , edBI
  -- , edMultilineSensitive
  -- , edMultilineInsensitive
  -- , edBlockSensitive
  -- , edBlockInsensitive
  -- , ed_ :: QuasiQuoter

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


------------------------------------------------------------------------
-- re Helpers
------------------------------------------------------------------------

re' :: Maybe SimpleREOptions -> QuasiQuoter
re' mb = case mb of
  Nothing  ->
    (qq0 "re'")
      { quoteExp = parse minBound (\rs->[|flip unsafeCompileRegex rs|])
      }
  Just sro ->
    (qq0 "re'")
      { quoteExp = parse sro (\rs->[|unsafeCompileRegexSimple sro rs|])
      }
  where
    parse :: SimpleREOptions -> (String->Q Exp) -> String -> Q Exp
    parse sro mk rs = either error (\_->mk rs) $ compileRegex_ os rs
      where
        os = unpackSimpleREOptions sro

unsafeCompileRegexSimple :: SimpleREOptions -> String -> RE
unsafeCompileRegexSimple sro re_s = unsafeCompileRegex_ os re_s
  where
    os = unpackSimpleREOptions sro

unsafeCompileRegex :: IsOption o RE CompOption ExecOption
                   => o
                   -> String
                   -> RE
unsafeCompileRegex = unsafeCompileRegex_ . makeREOptions

unsafeCompileRegex_ :: REOptions -> String -> RE
unsafeCompileRegex_ os = either oops id . compileRegex_ os
  where
    oops = error . ("unsafeCompileRegex: " ++)

compileRegex' :: (Functor m,Monad m)
              => REOptions
              -> String
              -> m (CaptureNames,Regex)
compileRegex' REOptions{..} s0 = do
    ((_,cnms),s2) <- either fail return $ extractNamedCaptures s1
    (,) cnms <$> makeRegexOptsM optionsComp optionsExec s2
  where
    s1 = expandMacros reSource optionsMacs s0

compileRegex_ :: (Functor m,Monad m)
              => REOptions
              -> String
              -> m RE
compileRegex_ os re_s = uncurry mk <$> compileRegex' os re_s
  where
    mk cnms rx =
      RE
        { _re_options = os
        , _re_source  = re_s
        , _re_cnames  = cnms
        , _re_regex   = rx
        }


------------------------------------------------------------------------
-- Options Helpers
------------------------------------------------------------------------

def_comp_option :: CompOption
def_comp_option = optionsComp defaultREOptions

def_exec_option :: ExecOption
def_exec_option = optionsExec defaultREOptions


------------------------------------------------------------------------
-- Haddock Sections
------------------------------------------------------------------------

-- $about
--
-- This module provides the regex PCRE back end. Most of the functions that
-- you will need for day to day use are provided by the primary API modules
-- (e.g., "Text.RE.TDFA.Text").
