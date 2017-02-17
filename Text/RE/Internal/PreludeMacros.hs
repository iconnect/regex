{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.Internal.PreludeMacros
  ( RegexType(..)
  , WithCaptures(..)
  , MacroDescriptor(..)
  , RegexSource(..)
  , PreludeMacro(..)
  , presentPreludeMacro
  , preludeMacros
  , preludeMacroTable
  , preludeMacroSummary
  , preludeMacroSources
  , preludeMacroSource
  , preludeMacroEnv
  , preludeMacroDescriptor
  ) where

import           Data.Array
import qualified Data.HashMap.Lazy            as HML
import           Data.List
import           Data.Maybe
import qualified Data.Text                    as T
import           Data.Time
import           Prelude.Compat
import           Text.RE.Options
import           Text.RE.Parsers
import           Text.RE.TestBench


preludeMacros :: (Monad m,Functor m)
              => (String->m r)
              -> RegexType
              -> WithCaptures
              -> m (Macros r)
preludeMacros prs rty wc = mkMacros prs rty wc $ preludeMacroEnv rty

preludeMacroTable :: RegexType -> String
preludeMacroTable rty = formatMacroTable rty $ preludeMacroEnv rty

preludeMacroSummary :: RegexType -> PreludeMacro -> String
preludeMacroSummary rty =
  formatMacroSummary rty (preludeMacroEnv rty) . prelude_macro_id

preludeMacroSources :: RegexType -> String
preludeMacroSources rty =
  formatMacroSources rty ExclCaptures $ preludeMacroEnv rty

preludeMacroSource :: RegexType -> PreludeMacro -> String
preludeMacroSource rty =
  formatMacroSource rty ExclCaptures (preludeMacroEnv rty) . prelude_macro_id

preludeMacroEnv :: RegexType -> MacroEnv
preludeMacroEnv TDFA = prelude_macro_env_tdfa
preludeMacroEnv PCRE = prelude_macro_env_pcre

prelude_macro_env_pcre :: MacroEnv
prelude_macro_env_pcre = fix $ prelude_macro_env PCRE

prelude_macro_env_tdfa :: MacroEnv
prelude_macro_env_tdfa = fix $ prelude_macro_env TDFA

prelude_macro_env :: RegexType -> MacroEnv -> MacroEnv
prelude_macro_env rty env = HML.fromList $ catMaybes
  [ (,) (prelude_macro_id pm) <$> preludeMacroDescriptor rty env pm
      | pm<-[minBound..maxBound]
      ]

preludeMacroDescriptor :: RegexType
                       -> MacroEnv
                       -> PreludeMacro
                       -> Maybe MacroDescriptor
preludeMacroDescriptor rty env pm = case pm of
  PM_nat          -> natural_macro          rty env pm
  PM_hex      -> natural_hex_macro      rty env pm
  PM_int          -> integer_macro          rty env pm
  PM_frac          -> decimal_macro          rty env pm
  PM_string           -> string_macro           rty env pm
  PM_string_simple    -> string_simple_macro    rty env pm
  PM_id               -> id_macro               rty env pm
  PM_id'              -> id'_macro              rty env pm
  PM_id_              -> id__macro              rty env pm
  PM_date             -> date_macro             rty env pm
  PM_date_slashes     -> date_slashes_macro     rty env pm
  PM_time             -> time_macro             rty env pm
  PM_timezone         -> timezone_macro         rty env pm
  PM_datetime         -> datetime_macro         rty env pm
  PM_datetime_8601    -> datetime_8601_macro    rty env pm
  PM_datetime_clf     -> datetime_clf_macro     rty env pm
  PM_shortmonth       -> shortmonth_macro       rty env pm
  PM_address_ipv4     -> address_ipv4_macros    rty env pm
  PM_email_simple     -> email_simple_macro     rty env pm
  PM_url              -> url_macro              rty env pm
  PM_syslog_severity  -> syslog_severity_macro  rty env pm

-- | an enumeration of all of the prelude macros
data PreludeMacro
  -- numbers
  = PM_nat
  | PM_hex
  | PM_int
  | PM_frac
  -- strings
  | PM_string
  | PM_string_simple
  -- identifiers
  | PM_id
  | PM_id'
  | PM_id_
  -- dates & times
  | PM_date
  | PM_date_slashes
  | PM_time
  | PM_timezone
  | PM_datetime
  | PM_datetime_8601
  | PM_datetime_clf
  | PM_shortmonth
  -- addresses
  | PM_address_ipv4
  | PM_email_simple
  | PM_url
  -- syslog
  | PM_syslog_severity
  deriving (Bounded,Enum,Ord,Eq,Show)

-- | naming the macros
presentPreludeMacro :: PreludeMacro -> String
presentPreludeMacro pm = case pm of
    PM_id_  -> prelude_prefix++"id-"
    _       -> fmt pm
  where
    fmt = (prelude_prefix++) . map tr . drop 3 . show

    tr '_' = '.'
    tr c   = c

-- | all prelude macros are prefixed with this
prelude_prefix :: String
prelude_prefix = "%"

prelude_macro_id :: PreludeMacro -> MacroID
prelude_macro_id = MacroID . presentPreludeMacro

natural_macro :: RegexType -> MacroEnv -> PreludeMacro -> Maybe MacroDescriptor
natural_macro rty env pm = Just $ run_tests rty parseInteger samples env pm
  MacroDescriptor
    { _md_source          = "[0-9]+"
    , _md_samples         = map fst samples
    , _md_counter_samples = counter_samples
    , _md_test_results    = []
    , _md_parser          = Just "parseInteger"
    , _md_description     = "a string of one or more decimal digits"
    }
  where
    samples :: [(String,Int)]
    samples =
      [ (,) "0"          0
      , (,) "1234567890" 1234567890
      , (,) "00"         0
      , (,) "01"         1
      ]

    counter_samples =
      [ ""
      , "0A"
      , "-1"
      ]

natural_hex_macro :: RegexType
                  -> MacroEnv
                  -> PreludeMacro
                  -> Maybe MacroDescriptor
natural_hex_macro rty env pm = Just $ run_tests rty parseHex samples env pm
  MacroDescriptor
    { _md_source          = "[0-9a-fA-F]+"
    , _md_samples         = map fst samples
    , _md_counter_samples = counter_samples
    , _md_test_results    = []
    , _md_parser          = Just "parseHex"
    , _md_description     = "a string of one or more hexadecimal digits"
    }
  where
    samples :: [(String,Int)]
    samples =
      [ (,) "0"         0x0
      , (,) "12345678"  0x12345678
      , (,) "90abcdef"  0x90abcdef
      , (,) "90ABCDEF"  0x90abcdef
      , (,) "00"        0x0
      , (,) "010"       0x10
      ]

    counter_samples =
      [ ""
      , "0x10"
      , "0z"
      , "-1a"
      ]

integer_macro :: RegexType -> MacroEnv -> PreludeMacro -> Maybe MacroDescriptor
integer_macro rty env pm = Just $ run_tests rty parseInteger samples env pm
  MacroDescriptor
    { _md_source          = "-?[0-9]+"
    , _md_samples         = map fst samples
    , _md_counter_samples = counter_samples
    , _md_test_results    = []
    , _md_parser          = Just "parseInteger"
    , _md_description     = "a decimal integer"
    }
  where
    samples :: [(String,Int)]
    samples =
      [ (,) "0"           0
      , (,) "1234567890"  1234567890
      , (,) "00"          0
      , (,) "01"          1
      , (,) "-1"       $ -1
      , (,) "-0"          0
      ]

    counter_samples =
      [ ""
      , "0A"
      , "+0"
      ]

-- | a digit string macro
decimal_macro :: RegexType -> MacroEnv -> PreludeMacro -> Maybe MacroDescriptor
decimal_macro rty env pm = Just $ run_tests rty parseDouble samples env pm
  MacroDescriptor
    { _md_source          = "-?[0-9]+(?:\\.[0-9]+)?"
    , _md_samples         = map fst samples
    , _md_counter_samples = counter_samples
    , _md_test_results    = []
    , _md_parser          = Just "parseInteger"
    , _md_description     = "a decimal integer"
    }
  where
    samples :: [(String,Double)]
    samples =
      [ (,) "0"             0
      , (,) "1234567890"    1234567890
      , (,) "00"            0
      , (,) "01"            1
      , (,) "-1"         $ -1
      , (,) "-0"            0
      , (,) "0.1234567890"  0.1234567890
      , (,) "-1.0"       $ -1.0
      ]

    counter_samples =
      [ ""
      , "0A"
      , "+0"
      , "0."
      , ".0"
      , "."
      , "-"
      , "-."
      , "-1."
      , "-.1"
      ]

string_macro :: RegexType
             -> MacroEnv
             -> PreludeMacro
             -> Maybe MacroDescriptor
string_macro     PCRE _  _   = Nothing
string_macro rty@TDFA env pm =
  Just $ run_tests rty (fmap T.unpack . parseString) samples env pm
    MacroDescriptor
      { _md_source          = "\"(?:[^\"\\]+|\\\\[\\\"])*\""
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Just "parseString"
      , _md_description     = "a double-quote string, with simple \\ escapes for \\s and \"s"
      }
  where
    samples :: [(String,String)]
    samples =
      [ (,) "\"\""                ""
      , (,) "\"foo\""             "foo"
      , (,) "\"\\\"\""            "\""
      , (,) "\"\\\"\\\"\""        "\"\""
      , (,) "\"\\\"\\\\\\\"\""    "\"\\\""
      , (,) "\"\\\"foo\\\"\""     "\"foo\""
      , (,) "\"\""                ""
      ]

    counter_samples =
      [ "\""
      , "\"aa"
      ]

string_simple_macro :: RegexType
                    -> MacroEnv
                    -> PreludeMacro
                    -> Maybe MacroDescriptor
string_simple_macro rty env pm =
  Just $ run_tests rty (fmap T.unpack . parseSimpleString) samples env pm
    MacroDescriptor
      { _md_source          = "\"[^\"[:cntrl:]]*\""
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Just "parseSimpleString"
      , _md_description     = "a decimal integer"
      }
  where
    samples :: [(String,String)]
    samples =
      [ (,) "\"\""      ""
      , (,) "\"foo\""   "foo"
      , (,) "\"\\\""    "\\"
      , (,) "\"\""      ""
      ]

    counter_samples =
      [ ""
      , "\""
      , "\"\\\"\""
      , "\"\\\"\\\"\""
      , "\"\\\"\\\\\\\"\""
      , "\"\\\"foo\\\"\""
      , "\"aa"
      ]

id_macro :: RegexType
         -> MacroEnv
         -> PreludeMacro
         -> Maybe MacroDescriptor
id_macro rty env pm =
  Just $ run_tests rty Just samples env pm
    MacroDescriptor
      { _md_source          = "_*[a-zA-Z][a-zA-Z0-9_]*"
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Nothing
      , _md_description     = "a standard C-style alphanumeric identifier (with _s)"
      }
  where
    samples :: [(String,String)]
    samples =
        [ f "a"
        , f "A"
        , f "A1"
        , f "a_"
        , f "a1_B2"
        , f "_abc"
        , f "__abc"
        ]
      where
        f s = (s,s)

    counter_samples =
        [ ""
        , "1"
        , "_"
        , "__"
        , "__1"
        , "1a"
        , "a'"
        ]

id'_macro :: RegexType
          -> MacroEnv
          -> PreludeMacro
          -> Maybe MacroDescriptor
id'_macro rty env pm =
  Just $ run_tests rty Just samples env pm
    MacroDescriptor
      { _md_source          = "_*[a-zA-Z][a-zA-Z0-9_']*"
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Nothing
      , _md_description     = "a standard Haskell-style alphanumeric identifier (with '_'s and '''s)"
      }
  where
    samples :: [(String,String)]
    samples =
        [ f "a"
        , f "A"
        , f "A1"
        , f "a_"
        , f "a1_B2"
        , f "_abc"
        , f "__abc"
        , f "a'"
        , f "_a'"
        , f "a'b"
        ]
      where
        f s = (s,s)

    counter_samples =
        [ ""
        , "1"
        , "_"
        , "__"
        , "__1"
        , "1a"
        , "'"
        , "'a"
        , "_'"
        , "_1'"
        ]

id__macro :: RegexType
          -> MacroEnv
          -> PreludeMacro
          -> Maybe MacroDescriptor
id__macro rty env pm =
  Just $ run_tests rty Just samples env pm
    MacroDescriptor
      { _md_source          = "_*[a-zA-Z][a-zA-Z0-9_'-]*"
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Nothing
      , _md_description     = "an identifier with -s"
      }
  where
    samples :: [(String,String)]
    samples =
        [ f "a"
        , f "A"
        , f "A1"
        , f "a_"
        , f "a1_B2"
        , f "_abc"
        , f "__abc"
        , f "a'"
        , f "_a'"
        , f "a'b"
        , f "a-"
        , f "a1-B2"
        , f "a1-B2-"
        ]
      where
        f s = (s,s)

    counter_samples =
        [ ""
        , "1"
        , "_"
        , "__"
        , "__1"
        , "1a"
        , "'"
        , "'a"
        , "_'"
        , "_1'"
        ]

date_macro :: RegexType -> MacroEnv -> PreludeMacro -> Maybe MacroDescriptor
date_macro rty env pm =
  Just $ run_tests rty parseDate samples env pm
    MacroDescriptor
      { _md_source          = "[0-9]{4}-[0-9]{2}-[0-9]{2}"
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Just "parseDate"
      , _md_description     = "a YYYY-MM-DD format date"
      }
  where
    samples :: [(String,Day)]
    samples =
        [ f "2016-12-31"
        , f "0001-01-01"
        , f "1000-01-01"
        ]
      where
        f s = (s,read s)

    counter_samples =
        [ ""
        , "2016/01/31"
        , "2016-1-31"
        , "2016-01-1"
        , "2016-001-01"
        ]

date_slashes_macro :: RegexType
                   -> MacroEnv
                   -> PreludeMacro
                   -> Maybe MacroDescriptor
date_slashes_macro rty env pm =
  Just $ run_tests rty parseSlashesDate samples env pm
    MacroDescriptor
      { _md_source          = "[0-9]{4}/[0-9]{2}/[0-9]{2}"
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Just "parseSlashesDate"
      , _md_description     = "a YYYY/MM/DD format date"
      }
  where
    samples :: [(String,Day)]
    samples =
        [ f "2016/12/31"
        , f "0001/01/01"
        , f "1000/01/01"
        ]
      where
        f s = (s,read $ map tr s)
          where
            tr '/' = '-'
            tr c   = c

    counter_samples =
        [ ""
        , "2016-01-31"
        , "2016/1/31"
        , "2016/01/1"
        , "2016/001/01"
        ]

time_macro :: RegexType -> MacroEnv -> PreludeMacro -> Maybe MacroDescriptor
time_macro rty env pm =
  Just $ run_tests rty parseTimeOfDay samples env pm
    MacroDescriptor
      { _md_source          = "[0-9]{2}:[0-9]{2}:[0-9]{2}(?:[.][0-9]+)?"
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Just "parseTimeOfDay"
      , _md_description     = "a HH:MM:SS[.Q+]"
      }
  where
    samples :: [(String,TimeOfDay)]
    samples =
        [ f "00:00:00"            00 00   0
        , f "23:59:59"            23 59   59
        , f "00:00:00.1234567890" 00 00 $ 123456789 / 1000000000
        ]
      where
        f s h m ps = (s,TimeOfDay h m ps)

    counter_samples =
        [ ""
        , "235959"
        , "10:20"
        , "A00:00:00"
        , "00:00:00A"
        , "23:59:59."
        ]

timezone_macro :: RegexType
               -> MacroEnv
               -> PreludeMacro
               -> Maybe MacroDescriptor
timezone_macro rty env pm =
  Just $ run_tests rty parseTimeZone samples env pm
    MacroDescriptor
      { _md_source          = "(?:Z|[+-][0-9]{2}:?[0-9]{2})"
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Just "parseTimeZone"
      , _md_description     = "an IOS-8601 TZ specification"
      }
  where
    samples :: [(String,TimeZone)]
    samples =
        [ f "Z"         $ minutesToTimeZone     0
        , f "+00:00"    $ minutesToTimeZone     0
        , f "+0000"     $ minutesToTimeZone     0
        , f "+0200"     $ minutesToTimeZone   120
        , f "-0100"     $ minutesToTimeZone $ -60
        ]
      where
        f = (,)

    counter_samples =
        [ ""
        , "00"
        , "A00:00"
        , "UTC"
        , "EST"
        , " EST"
        ]

datetime_macro :: RegexType -> MacroEnv -> PreludeMacro -> Maybe MacroDescriptor
datetime_macro rty env pm = Just $ run_tests rty parseDateTime samples env pm
  MacroDescriptor
    { _md_source          = "@{%date}[ T]@{%time}(?:@{%timezone}| UTC)?"
    , _md_samples         = map fst samples
    , _md_counter_samples = counter_samples
    , _md_test_results    = []
    , _md_parser          = Just "parseDateTime"
    , _md_description     = "ISO-8601 format date and time + simple variants"
    }
  where
    samples :: [(String,UTCTime)]
    samples =
        [ f "2016-12-31 23:37:22.525343 UTC" "2016-12-31 23:37:22.525343Z"
        , f "2016-12-31 23:37:22.525343"     "2016-12-31 23:37:22.525343Z"
        , f "2016-12-31 23:37:22"            "2016-12-31 23:37:22Z"
        , f "2016-12-31T23:37:22+0100"       "2016-12-31 23:37:22+0100"
        , f "2016-12-31T23:37:22-01:00"      "2016-12-31 23:37:22-0100"
        , f "2016-12-31T23:37:22-23:59"      "2016-12-31 23:37:22-2359"
        , f "2016-12-31T23:37:22Z"           "2016-12-31 23:37:22Z"
        ]
      where
        f :: String -> String -> (String,UTCTime)
        f s r_s = (s,read r_s)

    counter_samples =
        [ ""
        , "2016-12-31 23:37:22.525343 EST"
        ]

datetime_8601_macro :: RegexType
                    -> MacroEnv
                    -> PreludeMacro
                    -> Maybe MacroDescriptor
datetime_8601_macro rty env pm =
  Just $ run_tests rty parseDateTime samples env pm
    MacroDescriptor
      { _md_source          = "@{%date}T@{%time}@{%timezone}"
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Just "parseDateTime8601"
      , _md_description     = "YYYY-MM-DDTHH:MM:SS[.Q*](Z|[+-]HHMM) format date and time"
      }
  where
    samples :: [(String,UTCTime)]
    samples =
        [ f "2016-12-31T23:37:22.343Z"      "2016-12-31 23:37:22.343Z"
        , f "2016-12-31T23:37:22-0100"      "2016-12-31 23:37:22-0100"
        , f "2016-12-31T23:37:22+23:59"     "2016-12-31 23:37:22+2359"
        ]
      where
        f :: String -> String -> (String,UTCTime)
        f s r_s = (s,read r_s)

    counter_samples =
        [ ""
        , "2016-12-31 23:37:22.525343 EST"
        ]

datetime_clf_macro :: RegexType -> MacroEnv -> PreludeMacro -> Maybe MacroDescriptor
datetime_clf_macro rty env pm =
  Just $ run_tests rty parseDateTimeCLF samples env pm
    MacroDescriptor
      { _md_source          = re
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Just "parseDateTimeCLF"
      , _md_description     = "Common Log Format date+time: %d/%b/%Y:%H:%M:%S %z"
      }
  where
    samples :: [(String,UTCTime)]
    samples =
        [ f "10/Oct/2000:13:55:36 -0700"  "2000-10-10 13:55:36-0700"
        , f "10/Oct/2000:13:55:36 +07:00" "2000-10-10 13:55:36+0700"
        ]
      where
        f :: String -> String -> (String,UTCTime)
        f s r_s = (s,read r_s)

    counter_samples =
        [ ""
        , "2016-12-31T23:37+0100"
        , "10/Oct/2000:13:55:36-0700"
        , "10/OCT/2000:13:55:36 -0700"
        , "10/Oct/2000:13:55 -0700"
        , "10/Oct/2000:13:55Z"
        ]

    re = RegexSource $ unwords
      [ "[0-9]{2}/@{%shortmonth}/[0-9]{4}:[0-9]{2}:[0-9]{2}:[0-9]{2}"
      , "[+-][0-9]{2}:?[0-9]{2}"
      ]

shortmonth_macro :: RegexType
                 -> MacroEnv
                 -> PreludeMacro
                 -> Maybe MacroDescriptor
shortmonth_macro rty env pm =
  Just $ run_tests rty parseShortMonth samples env pm
    MacroDescriptor
      { _md_source          = bracketedRegexSource $
                                intercalate "|" $ map T.unpack $ elems shortMonthArray
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Just "parseShortMonth"
      , _md_description     = "three letter month name: Jan-Dec"
      }
  where
    samples :: [(String,Int)]
    samples =
        [ f "Jan"   1
        , f "Feb"   2
        , f "Dec"   12
        ]
      where
        f = (,)

    counter_samples =
        [ ""
        , "jan"
        , "DEC"
        , "January"
        , "01"
        , "1"
        ]

address_ipv4_macros :: RegexType
                    -> MacroEnv
                    -> PreludeMacro
                    -> Maybe MacroDescriptor
address_ipv4_macros rty env pm =
  Just $ run_tests rty parseIPv4Address samples env pm
    MacroDescriptor
      { _md_source          = "[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}"
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Just "parseSeverity"
      , _md_description     = "an a.b.c.d IPv4 address"
      }
  where
    samples :: [(String,IPV4Address)]
    samples =
        [ f "0.0.0.0"           (  0,  0,  0,  0)
        , f "123.45.6.78"       (123, 45,  6, 78)
        , f "9.9.9.9"           (  9,  9,  9,  9)
        , f "255.255.255.255"   (255,255,255,255)
        ]
      where
        f = (,)

    counter_samples =
        [ ""
        , "foo"
        , "1234.0.0.0"
        , "1.2.3"
        , "1.2.3."
        , "1.2..4"
        , "www.example.com"
        , "2001:0db8:85a3:0000:0000:8a2e:0370:7334"
        ]

syslog_severity_macro :: RegexType
                      -> MacroEnv
                      -> PreludeMacro
                      -> Maybe MacroDescriptor
syslog_severity_macro rty env pm =
  Just $ run_tests rty parseSeverity samples env pm
    MacroDescriptor
      { _md_source          = re
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Just "parseSeverity"
      , _md_description     = "syslog severity keyword (debug-emerg)"
      }
  where
    samples :: [(String,Severity)]
    samples =
        [ f "emerg"     Emerg
        , f "panic"     Emerg
        , f "alert"     Alert
        , f "crit"      Crit
        , f "err"       Err
        , f "error"     Err
        , f "warn"      Warning
        , f "warning"   Warning
        , f "notice"    Notice
        , f "info"      Info
        , f "debug"     Debug
        ]
      where
        f = (,)

    counter_samples =
        [ ""
        , "Emergency"
        , "ALERT"
        ]

    re = case rty of
      PCRE -> re_pcre
      TDFA -> re_tdfa

    re_tdfa = bracketedRegexSource $
          intercalate "|" $
            [ T.unpack kw
              | (kw0,kws) <- map severityKeywords [minBound..maxBound]
              , kw <- kw0:kws
              ]

    re_pcre = bracketedRegexSource $
          intercalate "|" $
            [ T.unpack kw
              | (kw0,kws) <- map severityKeywords $
                                  filter (/=Err) [minBound..maxBound]
              , kw <- kw0:kws
              ] ++ ["err(?:or)?"]

email_simple_macro :: RegexType
                   -> MacroEnv
                   -> PreludeMacro
                   -> Maybe MacroDescriptor
email_simple_macro rty env pm =
  Just $ run_tests rty Just samples env pm
    MacroDescriptor
      { _md_source          = "[a-zA-Z0-9%_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9.-]+"
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Nothing
      , _md_description     = "an email address"
      }
  where
    samples :: [(String,String)]
    samples =
        [ f "user-name%foo.bar.com@an-example.com"
        ]
      where
        f s = (s,s)

    counter_samples =
        [ ""
        , "not-an-email-address"
        , "@not-an-email-address"
        ]
-- | see https://mathiasbynens.be/demo/url-regex
-- (based on @stephenhay URL)
url_macro :: RegexType
          -> MacroEnv
          -> PreludeMacro
          -> Maybe MacroDescriptor
url_macro rty env pm =
  Just $ run_tests rty Just samples env pm
    MacroDescriptor
      { _md_source          = "([hH][tT][tT][pP][sS]?|[fF][tT][pP])://[^[:space:]/$.?#].[^[:space:]]*"
      , _md_samples         = map fst samples
      , _md_counter_samples = counter_samples
      , _md_test_results    = []
      , _md_parser          = Nothing
      , _md_description     = "a URL"
      }
  where
    samples :: [(String,String)]
    samples =
        [ f "https://mathiasbynens.be/demo/url-regex"
        , f "http://foo.com/blah_blah"
        , f "http://foo.com/blah_blah/"
        , f "http://foo.com/blah_blah_(wikipedia)"
        , f "http://foo.com/blah_blah_(wikipedia)_(again)"
        , f "http://www.example.com/wpstyle/?p=364"
        , f "HTTPS://foo.bar/?q=Test%20URL-encoded%20stuff"
        , f "HTTP://223.255.255.254"
        , f "ftp://223.255.255.254"
        , f "FTP://223.255.255.254"
        ]
      where
        f s = (s,s)

    counter_samples =
        [ ""
        , "http://"
        , "http://."
        , "http://.."
        , "http://../"
        , "http://?"
        , "http://??"
        , "http://foo.bar?q=Spaces should be encoded"
        , "//"
        , "http://##/"
        , "http://##"
        , "http://##/"
        ]

run_tests :: (Eq a,Show a)
          => RegexType
          -> (String->Maybe a)
          -> [(String,a)]
          -> MacroEnv
          -> PreludeMacro
          -> MacroDescriptor
          -> MacroDescriptor
run_tests rty parser vector env =
  runTests rty parser vector env . prelude_macro_id

bracketedRegexSource :: String -> RegexSource
bracketedRegexSource re_s = RegexSource $ "(?:" ++ re_s ++ ")"

fix :: (a->a) -> a
fix f = f (fix f)
