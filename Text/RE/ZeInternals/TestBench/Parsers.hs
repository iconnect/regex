{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE CPP                                #-}

module Text.RE.ZeInternals.TestBench.Parsers
  (
  -- * The Test Bench
    MacroEnv
  , MacroDescriptor(..)
  , RegexSource(..)
  , WithCaptures(..)
  , RegexType
  , isTDFA
  , isPCRE
  , presentRegexType
  -- ** Constructing a MacrosEnv
  , mkMacros
  -- ** Formatting Macros
  , formatMacroTable
  , formatMacroSummary
  , formatMacroSources
  , formatMacroSource
  -- ** Formatting Macros
  , testMacroEnv
  , runTests
  , runTests'
  -- * Parsing
  , parseInteger
  , parseHex
  , parseDouble
  , parseString
  , parseSimpleString
  , parseDate
  , parseSlashesDate
  , parseTimeOfDay
  , parseTimeZone
  , parseDateTime
  , parseDateTime8601
  , parseDateTimeCLF
  , parseShortMonth
  , shortMonthArray
  , IPV4Address
  , parseIPv4Address
  , Severity(..)
  , parseSeverity
  , severityKeywords
  ) where

import           Data.Array
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Time
import qualified Data.Time.Locale.Compat    as LC
import           Data.Word
import           Text.Printf
import           Text.RE.Replace
import           Text.RE.ZeInternals.TestBench
import           Text.Read


parseInteger :: Replace a => a -> Maybe Int
parseInteger = readMaybe . unpackR

parseHex :: Replace a => a -> Maybe Int
parseHex = readMaybe . ("0x"++) . unpackR

parseDouble :: Replace a => a -> Maybe Double
parseDouble = readMaybe . unpackR

parseString :: Replace a => a -> Maybe T.Text
parseString = readMaybe . unpackR

parseSimpleString :: Replace a => a -> Maybe T.Text
parseSimpleString = Just . T.dropEnd 1 . T.drop 1 . textifyR

date_templates, time_templates, timezone_templates,
  date_time_8601_templates, date_time_templates :: [String]
date_templates            = ["%F"]
time_templates            = ["%H:%M:%S","%H:%M:%S%Q","%H:%M"]
timezone_templates        = ["Z","%z"]
date_time_8601_templates  =
    [ printf "%sT%s%s" dt tm tz
        | dt <- date_templates
        , tm <- time_templates
        , tz <- timezone_templates
        ]
date_time_templates       =
    [ printf "%s%c%s%s" dt sc tm tz
        | dt <- date_templates
        , sc <- ['T',' ']
        , tm <- time_templates
        , tz <- timezone_templates ++ [" UTC",""]
        ]

parseDate :: Replace a => a -> Maybe Day
parseDate = parse_time date_templates

parseSlashesDate :: Replace a => a -> Maybe Day
parseSlashesDate = parse_time ["%Y/%m/%d"]

parseTimeOfDay :: Replace a => a -> Maybe TimeOfDay
parseTimeOfDay = parse_time time_templates

parseTimeZone :: Replace a => a -> Maybe TimeZone
parseTimeZone = parse_time timezone_templates

parseDateTime :: Replace a => a -> Maybe UTCTime
parseDateTime = parse_time date_time_templates

parseDateTime8601 :: Replace a => a -> Maybe UTCTime
parseDateTime8601 = parse_time date_time_8601_templates

parseDateTimeCLF :: Replace a => a -> Maybe UTCTime
parseDateTimeCLF = parse_time ["%d/%b/%Y:%H:%M:%S %z"]

parseShortMonth :: Replace a => a -> Maybe Int
parseShortMonth = flip HM.lookup short_month_hm . unpackR

parse_time :: (ParseTime t,Replace s) => [String] -> s -> Maybe t
parse_time tpls = prs . unpackR
  where
    prs s = listToMaybe $ catMaybes
      [ parseTimeM True  LC.defaultTimeLocale fmt s
          | fmt<-tpls
          ]
#if !MIN_VERSION_time(1,5,0)
    parseTimeM _ = parseTime
#endif

short_month_hm :: HM.HashMap String Int
short_month_hm = HM.fromList [ (T.unpack $ shortMonthArray!i,i) | i<-[1..12] ]

shortMonthArray :: Array Int T.Text
shortMonthArray = listArray (1,12) $
  T.words "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec"

type IPV4Address = (Word8,Word8,Word8,Word8)

parseIPv4Address :: Replace a => a -> Maybe IPV4Address
parseIPv4Address = prs . words_by (=='.') . unpackR
  where
    prs [a_s,b_s,c_s,d_s] = do
      a <- readMaybe a_s
      b <- readMaybe b_s
      c <- readMaybe c_s
      d <- readMaybe d_s
      case all is_o [a,b,c,d] of
        True  -> Just (toEnum a,toEnum b,toEnum c,toEnum d)
        False -> Nothing
    prs _ = Nothing

    is_o x = 0 <= x && x <= 255

data Severity
  = Emerg
  | Alert
  | Crit
  | Err
  | Warning
  | Notice
  | Info
  | Debug
  deriving (Bounded,Enum,Ord,Eq,Show)

parseSeverity :: Replace a => a -> Maybe Severity
parseSeverity = flip HM.lookup severity_hm . textifyR

severity_hm :: HM.HashMap T.Text Severity
severity_hm = HM.fromList
  [ (kw,pri)
      | pri<-[minBound..maxBound]
      , let (kw0,kws) = severityKeywords pri
      , kw <- kw0:kws
      ]

severityKeywords :: Severity -> (T.Text,[T.Text])
severityKeywords pri = case pri of
  Emerg     -> (,) "emerg"    ["panic"]
  Alert     -> (,) "alert"    []
  Crit      -> (,) "crit"     []
  Err       -> (,) "err"      ["error"]
  Warning   -> (,) "warning"  ["warn"]
  Notice    -> (,) "notice"   []
  Info      -> (,) "info"     []
  Debug     -> (,) "debug"    []

words_by :: (Char->Bool) -> String -> [String]
words_by f s = case dropWhile f s of
  "" -> []
  s' -> w : words_by f s''
        where
          (w, s'') = break f s'
