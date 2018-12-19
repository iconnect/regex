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

module Text.RE.ZeInternals.TDFA
  ( -- * About
    -- $about

    -- * RE Type
    RE
  , regexType
  , reOptions
  , reSource
  , reCaptureNames
  , reRegex
  -- * IsOptions Class and REOptions Type
  , IsOption(..)
  , REOptions
  , defaultREOptions
  , noPreludeREOptions
  , unpackSimpleREOptions
  -- * Compiling Regular Expressions
  , compileRegex
  , compileRegexWith
  , compileRegexWithOptions
  , compileRegexWithOptionsForQQ
  -- * Compiling Search-Replace Templates
  , compileSearchReplace
  , compileSearchReplaceWith
  , compileSearchReplaceWithOptions
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
import           Text.RE.REOptions
import           Text.RE.Replace
import           Text.RE.TestBench
import           Text.RE.Tools
import           Text.RE.ZeInternals
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
-- for this
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
-- IsOption & REOptions
------------------------------------------------------------------------

-- | a number of types can be used to encode 'REOptions_', each of which
-- is made a member of this class
class IsOption o where
  -- | convert the @o@ type into an @REOptions@
  makeREOptions :: o -> REOptions

-- | and the REOptions for this back end (see "Text.RE.REOptions"
-- for details)
type REOptions = REOptions_ RE CompOption ExecOption

instance IsOption SimpleREOptions where
  makeREOptions    = unpackSimpleREOptions

instance IsOption (Macros RE) where
  makeREOptions ms = REOptions ms def_comp_option def_exec_option

instance IsOption CompOption where
  makeREOptions co = REOptions prelude co def_exec_option

instance IsOption ExecOption where
  makeREOptions eo = REOptions prelude def_comp_option eo

instance IsOption REOptions where
  makeREOptions    = id

instance IsOption () where
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
compileRegex = compileRegexWith minBound

-- | compile a 'String' into a 'RE' using the given @SimpleREOptions@,
-- generating an error if the RE is not well formed
compileRegexWith :: (Functor m,Monad m) => SimpleREOptions -> String -> m RE
compileRegexWith = compileRegexWithOptions

-- | compile a 'String' into a 'RE' using the given @SimpleREOptions@,
-- generating an error if the RE is not well formed
compileRegexWithOptions :: (IsOption o, Functor m, Monad   m)
                        => o
                        -> String
                        -> m RE
compileRegexWithOptions = compileRegex_ RPM_raw . makeREOptions

-- | compile a 'String' into a 'RE' for q quasi quoter, using the given
-- @SimpleREOptions@, generating an error if the RE is not well formed
compileRegexWithOptionsForQQ :: (IsOption o, Functor m, Monad   m)
                             => o
                             -> String
                             -> m RE
compileRegexWithOptionsForQQ = compileRegex_ RPM_qq . makeREOptions


------------------------------------------------------------------------
-- Compiling Search Replace Templates
------------------------------------------------------------------------

-- | compile a SearchReplace template generating errors if the RE or
-- the template are not well formed, all capture references being checked
compileSearchReplace :: (Monad m,Functor m,IsRegex RE s)
                     => String
                     -> String
                     -> m (SearchReplace RE s)
compileSearchReplace = compileSearchReplaceWith minBound

-- | compile a SearchReplace template, with simple options, generating
-- errors if the RE or the template are not well formed, all capture
-- references being checked
compileSearchReplaceWith :: (Monad m,Functor m,IsRegex RE s)
                         => SimpleREOptions
                         -> String
                         -> String
                         -> m (SearchReplace RE s)
compileSearchReplaceWith sro = compileSearchAndReplace_ packR $ compileRegexWith sro

-- | compile a SearchReplace template, with general options, generating
-- errors if the RE or the template are not well formed, all capture
-- references being checked
compileSearchReplaceWithOptions :: (Monad m,Functor m,IsRegex RE s)
                                => REOptions
                                -> String
                                -> String
                                -> m (SearchReplace RE s)
compileSearchReplaceWithOptions os = compileSearchAndReplace_ packR $ compileRegexWithOptions os


------------------------------------------------------------------------
-- Escaping Strings
------------------------------------------------------------------------

-- | convert a string into a RE that matches that string, and apply it
-- to an argument continuation function to make up the RE string to be
-- compiled; e.g., to compile a RE that will only match the string:
--
--  @maybe undefined id . escape ((\"^\"++) . (++\"$\"))@
--
escape :: (Functor m,Monad m)
       => (String->String)
       -> String
       -> m RE
escape = escapeWith minBound

-- | a variant of 'escape' where the 'SimpleREOptions' are specified
escapeWith :: (Functor m,Monad m)
           => SimpleREOptions
           -> (String->String)
           -> String
           -> m RE
escapeWith = escapeWithOptions

-- | a variant of 'escapeWith' that allows an 'IsOption' RE option
-- to be specified
escapeWithOptions :: ( IsOption o, Functor m, Monad m)
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
    mk = Identity . unsafeCompileRegex_ RPM_raw noPreludeREOptions

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

-- | @[re| ... |]@, is equivalent to @[reMultilineSensitive| ... |]@,
-- compiling a case-sensitive, multi-line RE
re                      :: QuasiQuoter
re                       = re' $ Just minBound

-- | @[reMultilineSensitive| ... |]@, compiles a case-sensitive, multi-line RE
reMultilineSensitive    :: QuasiQuoter
reMultilineSensitive     = re' $ Just  MultilineSensitive

-- | @[reMultilineInsensitive| ... |]@, compiles a case-insensitive, multi-line RE
reMultilineInsensitive  :: QuasiQuoter
reMultilineInsensitive   = re' $ Just  MultilineInsensitive

-- | @[reMultilineInsensitive| ... |]@, compiles a case-sensitive, non-multi-line RE
reBlockSensitive        :: QuasiQuoter
reBlockSensitive         = re' $ Just  BlockSensitive

-- | @[reMultilineInsensitive| ... |]@, compiles a case-insensitive, non-multi-line RE
reBlockInsensitive      :: QuasiQuoter
reBlockInsensitive       = re' $ Just  BlockInsensitive

-- | @[reMS| ... |]@ is a shorthand for @[reMultilineSensitive| ... |]@
reMS                     :: QuasiQuoter
reMS                     = reMultilineSensitive

-- | @[reMI| ... |]@ is a shorthand for @[reMultilineInsensitive| ... |]@
reMI                    :: QuasiQuoter
reMI                     = reMultilineInsensitive

-- | @[reBS| ... |]@ is a shorthand for @[reBlockSensitive| ... |]@
reBS                    :: QuasiQuoter
reBS                     = reBlockSensitive

-- | @[reBI| ... |]@ is a shorthand for @[reBlockInsensitive| ... |]@
reBI                    :: QuasiQuoter
reBI                     = reBlockInsensitive

-- | @[re_| ... |]@ compiles a RE to produce a function that takes
-- the RE options (e.g., a 'SimpleREOptions' value) and yields the
-- RE compiled with those options. For example,
--
--   @countMatches $ s *=~ [re_|[0-9a-f]+|] MultilineInsensitive@
--
-- counts the number of hexadecimal digit strings in 's', allowing
-- for upper- or lower-case hex didgits (which is entirely equivalent
-- in this example to just using @[reMultilineInsensitive|[0-9a-f]+|]@).
re_                     :: QuasiQuoter
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
    parse sro mk rs = either error (\_->mk rs) $ compileRegex_ RPM_qq os rs
      where
        os = unpackSimpleREOptions sro

data RegexParseMode
  = RPM_qq
  | RPM_raw
  deriving (Eq,Show)

unsafeCompileRegexSimple :: SimpleREOptions -> String -> RE
unsafeCompileRegexSimple sro re_s = unsafeCompileRegex_ RPM_qq os re_s
  where
    os = unpackSimpleREOptions sro

unsafeCompileRegex :: IsOption o
                   => o
                   -> String
                   -> RE
unsafeCompileRegex = unsafeCompileRegex_ RPM_qq . makeREOptions

unsafeCompileRegex_ :: RegexParseMode -> REOptions -> String -> RE
unsafeCompileRegex_ rpm os = either oops id . compileRegex_ rpm os
  where
    oops = error . ("unsafeCompileRegex: " ++)

compileRegex_ :: (Functor m,Monad m)
              => RegexParseMode
              -> REOptions
              -> String
              -> m RE
compileRegex_ rpm os re_s = uncurry mk <$> compileRegex' rpm os re_s
  where
    mk cnms rx =
      RE
        { _re_options = os
        , _re_source  = re_s
        , _re_cnames  = cnms
        , _re_regex   = rx
        }

compileRegex' :: (Functor m,Monad m)
              => RegexParseMode
              -> REOptions
              -> String
              -> m (CaptureNames,Regex)
compileRegex' rpm REOptions{..} s0 = do
    ((_,cnms),s2) <- either fail return $ extractNamedCaptures s1
    (,) cnms <$> makeRegexOptsM optionsComp optionsExec s2
  where
    s1 = expandMacros reSource optionsMacs $ pp s0

    pp = case rpm of
      RPM_qq  -> qq_prep
      RPM_raw -> id


------------------------------------------------------------------------
-- Preprocessing Literal REs
------------------------------------------------------------------------

qq_prep :: String -> String
qq_prep s0 = case s0 of
    ""  -> ""
    c:s -> case c of
      '\\' -> backslash s
      _    -> c : qq_prep s
  where
    backslash s1 = case s1 of
      ""  -> "\\"
      c:s -> case c of
        'a'  -> '\a'    : qq_prep s
        'b'  -> '\b'    : qq_prep s
        'f'  -> '\f'    : qq_prep s
        'n'  -> '\n'    : qq_prep s
        'r'  -> '\r'    : qq_prep s
        't'  -> '\t'    : qq_prep s
        'v'  -> '\v'    : qq_prep s
        _    -> '\\': c : qq_prep s


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
