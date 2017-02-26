\begin{code}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RecordWildCards            #-}

module Text.RE.Internal.NamedCaptures
  ( cp
  , extractNamedCaptures
  , idFormatTokenOptions
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
import           Text.Heredoc
import           Text.RE
import           Text.RE.Internal.PreludeMacros
import           Text.RE.Internal.QQ
import           Text.Regex.PCRE


cp :: QuasiQuoter
cp =
    (qq0 "re_")
      { quoteExp = parse_capture
      }

extractNamedCaptures :: String -> Either String (CaptureNames,String)
extractNamedCaptures s = Right (analyseTokens tks,formatTokens tks)
  where
    tks = scan s
\end{code}


Token
-----

\begin{code}
data Token
  = ECap (Maybe String)
  | PGrp
  | PCap
  | Bra
  | BS          Char
  | Other       Char
  deriving (Show,Generic,Eq)

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
analyseTokens :: [Token] -> CaptureNames
analyseTokens = HM.fromList . count_em 1
  where
    count_em _ []       = []
    count_em n (tk:tks) = bd ++ count_em (n `seq` n+d) tks
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
scan :: String -> [Token]
scan = alex' match al oops
  where
    al :: [(Regex,Match String->Maybe Token)]
    al =
      [ mk [here|\$\{([^{}]+)\}\(|] $         ECap . Just . x_1
      , mk [here|\$\(|]             $ const $ ECap Nothing
      , mk [here|\(\?:|]            $ const   PGrp
      , mk [here|\(\?|]             $ const   PCap
      , mk [here|\(|]               $ const   Bra
      , mk [here|\\(.)|]            $         BS    . s2c . x_1
      , mk [here|(.)|]              $         Other . s2c . x_1
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
formatTokens :: [Token] -> String
formatTokens = formatTokens' defFormatTokenOptions

data FormatTokenOptions =
  FormatTokenOptions
    { _fto_regex_type :: Maybe RegexType
    , _fto_min_caps   :: Bool
    , _fto_incl_caps  :: Bool
    }
  deriving (Show)

defFormatTokenOptions :: FormatTokenOptions
defFormatTokenOptions =
  FormatTokenOptions
    { _fto_regex_type = Nothing
    , _fto_min_caps   = False
    , _fto_incl_caps  = False
    }

idFormatTokenOptions :: FormatTokenOptions
idFormatTokenOptions =
  FormatTokenOptions
    { _fto_regex_type     = Nothing
    , _fto_min_caps       = False
    , _fto_incl_caps = True
    }

formatTokens' :: FormatTokenOptions -> [Token] -> String
formatTokens' FormatTokenOptions{..} = foldr f ""
  where
    f tk tl = t_s ++ tl
      where
        t_s = case tk of
          ECap  mb -> ecap mb
          PGrp     -> if _fto_regex_type == Just TDFA then "(" else "(?:"
          PCap     -> "(?"
          Bra      -> bra _fto_min_caps
          BS    c  -> "\\" ++ [c]
          Other c  -> [c]

    ecap mb = case _fto_incl_caps of
      True  -> case mb of
        Nothing -> "$("
        Just nm -> "${"++nm++"}("
      False -> bra _fto_min_caps

    bra mc  = case mc && _fto_regex_type == Just PCRE of
      True  -> "(?:"
      False -> "("
\end{code}

\begin{code}
-- this is a reference of formatTokens defFormatTokenOptions,
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
