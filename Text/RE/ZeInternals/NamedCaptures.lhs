\begin{code}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes      #-}
#else
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
#endif

module Text.RE.ZeInternals.NamedCaptures
  ( cp
  , extractNamedCaptures
  , idFormatTokenREOptions
  , Token(..)
  , validToken
  , formatTokens
  , formatTokens'
  , formatTokens0
  , scan
  ) where

import           Data.Char
import qualified Data.HashMap.Strict          as HM
import qualified Data.Text                    as T
import           GHC.Generics
import qualified Language.Haskell.TH          as TH
import           Language.Haskell.TH.Quote
import           Text.RE
import           Text.RE.ZeInternals.PreludeMacros
import           Text.RE.ZeInternals.QQ
import           Text.RE.ZeInternals.TestBench
import           Text.RE.Tools.Lex
import           Text.RE.ZeInternals.Types.CaptureID
import           Text.RE.ZeInternals.Types.Match
import           Text.Regex.TDFA


-- | quasi quoter for CaptureID ([cp|0|],[cp|y|], etc.)
cp :: QuasiQuoter
cp =
    (qq0 "cp")
      { quoteExp = parse_capture
      }

-- | extract the CaptureNames from an RE or return an error diagnostic
-- if the RE is not well formed; also returs the total number of captures
-- in the RE
extractNamedCaptures :: String -> Either String ((Int,CaptureNames),String)
extractNamedCaptures s = Right (analyseTokens tks,formatTokens tks)
  where
    tks = scan s
\end{code}


Token
-----

\begin{code}
-- | our RE scanner returns a list of these tokens
data Token
  = ECap (Maybe String)
  | PGrp
  | PCap
  | Bra
  | BS          Char
  | Other       Char
  deriving (Show,Generic,Eq)

-- | check that a token is well formed
validToken :: Token -> Bool
validToken tkn = case tkn of
    ECap  mb -> maybe True check_ecap mb
    PGrp     -> True
    PCap     -> True
    Bra      -> True
    BS    c  -> is_dot c
    Other c  -> is_dot c
  where
    check_ecap s = not (null s) && all not_br s
    is_dot     c = c/='\n'
    not_br     c = not $ c `elem` "{}\n"

\end{code}


Analysing [Token] -> CaptureNames
---------------------------------

\begin{code}
-- | analyse a token stream, returning the number of captures and the
-- 'CaptureNames'
analyseTokens :: [Token] -> (Int,CaptureNames)
analyseTokens tks0 = case count_em 1 tks0 of
    (n,as) -> (n-1, HM.fromList as)
  where
    count_em n []       = (n,[])
    count_em n (tk:tks) = case count_em (n `seq` n+d) tks of
        (n',as) -> (n',bd++as)
      where
        (d,bd) = case tk of
          ECap (Just nm) -> (,) 1 [(CaptureName $ T.pack nm,CaptureOrdinal n)]
          ECap  Nothing  -> (,) 1 []
          PGrp           -> (,) 0 []
          PCap           -> (,) 1 []
          Bra            -> (,) 1 []
          BS    _        -> (,) 0 []
          Other _        -> (,) 0 []
\end{code}


Scanning Regex Strings
----------------------

\begin{code}
-- | scan a RE string into a list of RE Token
scan :: String -> [Token]
scan = alex' match al oops
  where
    al :: [(Regex,Match String->Maybe Token)]
    al =
      [ mk "\\$\\{([^{}]+)\\}\\(" $         ECap . Just . x_1
      , mk "\\$\\("               $ const $ ECap Nothing
      , mk "\\(\\?:"              $ const   PGrp
      , mk "\\(\\?"               $ const   PCap
      , mk "\\("                  $ const   Bra
      , mk "\\\\(.)"              $         BS    . s2c . x_1
      , mk "(.)"                  $         Other . s2c . x_1
      ]

    x_1     = captureText $ IsCaptureOrdinal $ CaptureOrdinal 1

    s2c [c] = c
    s2c _   = error "scan:s2c:internal error"

    mk s f  = (either error id $ makeRegexM s,Just . f)

    oops    = error "reScanner"
\end{code}


Parsing captures
----------------

\begin{code}
parse_capture :: String -> TH.Q TH.Exp
parse_capture s = case all isDigit s of
  True  -> [|IsCaptureOrdinal $ CaptureOrdinal $ read s|]
  False -> [|IsCaptureName    $ CaptureName $ T.pack  s|]
\end{code}


Formatting [Token]
------------------

\begin{code}
-- | format [Token] into an RE string
formatTokens :: [Token] -> String
formatTokens = formatTokens' defFormatTokenREOptions

-- | options for the general Token formatter below
data FormatTokenREOptions =
  FormatTokenREOptions
    { _fto_regex_type :: Maybe RegexType    -- ^ Posix, PCRE or indeterminate REs?
    , _fto_min_caps   :: Bool               -- ^ remove captures where possible
    , _fto_incl_caps  :: Bool               -- ^ include the captures in the output
    }
  deriving (Show)

-- | the default configuration for the Token formatter
defFormatTokenREOptions :: FormatTokenREOptions
defFormatTokenREOptions =
  FormatTokenREOptions
    { _fto_regex_type = Nothing
    , _fto_min_caps   = False
    , _fto_incl_caps  = False
    }

-- | a configuration that will preserve the parsed regular expression
-- in the output
idFormatTokenREOptions :: FormatTokenREOptions
idFormatTokenREOptions =
  FormatTokenREOptions
    { _fto_regex_type = Nothing
    , _fto_min_caps   = False
    , _fto_incl_caps  = True
    }

-- | the general Token formatter, generating REs according to the options
formatTokens' :: FormatTokenREOptions -> [Token] -> String
formatTokens' FormatTokenREOptions{..} = foldr f ""
  where
    f tk tl = t_s ++ tl
      where
        t_s = case tk of
          ECap  mb -> ecap mb
          PGrp     -> if maybe False isTDFA _fto_regex_type then "(" else "(?:"
          PCap     -> "(?"
          Bra      -> bra _fto_min_caps
          BS    c  -> "\\" ++ [c]
          Other c  -> [c]

    ecap mb = case _fto_incl_caps of
      True  -> case mb of
        Nothing -> "$("
        Just nm -> "${"++nm++"}("
      False -> bra _fto_min_caps

    bra mc  = case mc && maybe False isPCRE _fto_regex_type of
      True  -> "(?:"
      False -> "("
\end{code}

\begin{code}
-- this is a reference of formatTokens defFormatTokenREOptions,
-- used for testing the latter
formatTokens0 :: [Token] -> String
formatTokens0 = foldr f ""
  where
    f tk tl = t_s ++ tl
      where
        t_s = case tk of
          ECap  _ -> "("
          PGrp    -> "(?:"
          PCap    -> "(?"
          Bra     -> "("
          BS    c -> "\\" ++ [c]
          Other c -> [c]
\end{code}
