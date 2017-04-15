Example: NGINX Log Processor
============================

This example program reads lines from NGINX error-log files and
access-log files converts them into a unified output format.

It is an example of developing REs at scale using macros with
the regex test bench.

The tool is self-testing: run it with no arguments (or `cabal test`).


\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main
  ( main
  -- development
  , parse_a
  , parse_e
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8               as LBS
import           Data.Char
import           Data.Functor.Identity
import qualified Data.HashMap.Lazy                        as HML
import           Data.Maybe
import           Data.String
import qualified Data.Text                                as T
import           Data.Time
import           Prelude.Compat
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           TestKit
import           Text.Printf
import qualified Text.RE.PCRE                             as PCRE
import           Text.RE.PCRE.ByteString.Lazy
import qualified Text.RE.PCRE.String                      as S
import           Text.RE.REOptions
import           Text.RE.Replace
import           Text.RE.TestBench
import           Text.RE.Tools.Sed
\end{code}

\begin{code}
main :: IO ()
main = do
  as  <- getArgs
  case as of
    ["--macro"      ] -> putStr      lp_macro_table
    ["--macro",mid_s] -> putStrLn  $ lp_macro_summary $ MacroID mid_s
    ["--regex"      ] -> putStr      lp_macro_sources
    ["--regex",mid_s] -> putStrLn  $ lp_macro_source  $ MacroID mid_s
    ["--test"       ] -> test
    []                -> test
    [in_file          ] | is_file in_file -> go True  in_file "-"
    [in_file,out_file ] | is_file in_file -> go True  in_file out_file
    _                                     -> usage
  where
    is_file = not . (== "--") . take 2

    usage = do
      pnm <- getProgName
      let prg = ((pnm++" ")++)
      putStr $ unlines
        [ "usage:"
        , prg " --help"
        , prg " --macro"
        , prg " --macro <macro-id>"
        , prg " --regex"
        , prg " --regex <macro-id>"
        , prg "[--test]"
        , prg "(-|<in-file>) [-|<out-file>]"
        ]
\end{code}

\begin{code}

--
-- go
--

test :: IO ()
test = do
    putStrLn "============================================================"
    putStrLn "Testing the macro environment."
    putStrLn "nginx-log-processor"
    is_docs <- doesDirectoryExist "docs"
    when is_docs $
      dumpMacroTable  (fp "docs" ".txt") (fp "docs" "-src.txt") regexType lp_env
    dumpMacroTable    (fp "data" ".txt") (fp "data" "-src.txt") regexType lp_env
    me_ok <- testMacroEnv "nginx-log-processor" regexType lp_env
    putStrLn "============================================================"
    putStrLn "Testing the log processor on reference data."
    putStrLn ""
    lp_ok <- test_log_processor
    putStrLn "============================================================"
    case me_ok && lp_ok of
      True  -> return ()
      False -> exitWith $ ExitFailure 1
  where
    fp dir sfx = dir </> (rty_s ++ "-nginx-log-processor" ++ sfx)

    rty_s      = map toLower $ presentRegexType regexType

test_log_processor :: IO Bool
test_log_processor = do
    createDirectoryIfMissing False "tmp"
    go False "data/access-errors.log" "tmp/events.log"
    cmp "tmp/events.log" "data/events.log"
\end{code}

\begin{code}

--
-- go
--

go :: Bool -> FilePath -> FilePath -> IO ()
go rprt_flg in_file out_file = do
  ctx <- setup rprt_flg
  sed (script ctx) in_file out_file
\end{code}

\begin{code}
script :: Ctx -> Edits IO RE LBS.ByteString
script ctx = Select
    [ on [re_|@{access}|]     ACC parse_access
    , on [re_|@{access_deg}|] AQQ parse_deg_access
    , on [re_|@{error}|]      ERR parse_error
    , on [re_|.*|]            QQQ parse_def
    ]
  where
    on rex src prs = Function (rex lpo) TOP $ process_line ctx src prs

    parse_def      = fmap capturedText . matchCapture
\end{code}

\begin{code}
process_line :: IsEvent a
             => Ctx
             -> Source
             -> (Match LBS.ByteString->Maybe a)
             -> LineNo
             -> Match LBS.ByteString
             -> RELocation
             -> Capture LBS.ByteString
             -> IO (Maybe LBS.ByteString)
process_line ctx src prs lno cs _ _ = do
    when (event_is_notifiable event) $
      flag_event ctx event
    return $ Just $ presentEvent event
  where
    event     = maybe def_event (mkEvent lno src) $ prs cs

    def_event =
      Event
        { _event_line     = lno
        , _event_source   = src
        , _event_utc      = read "1970-01-01 00:00:00"
        , _event_severity = Nothing
        , _event_address  = (0,0,0,0)
        , _event_details  = ""
        }


--
-- Ctx, setup, event_is_notifiable, flag_event
--

type Ctx = Bool

setup :: Bool -> IO Ctx
setup = return

event_is_notifiable :: Event -> Bool
event_is_notifiable Event{..} =
  fromEnum (fromMaybe Debug _event_severity) <= fromEnum Err

flag_event :: Ctx -> Event -> IO ()
flag_event False = const $ return ()
flag_event True  = LBS.hPutStrLn stderr . presentEvent


--
-- Event, presentEvent, IsEvent
--

data Event =
  Event
    { _event_line     :: LineNo
    , _event_source   :: Source
    , _event_utc      :: UTCTime
    , _event_severity :: Maybe Severity
    , _event_address  :: IPV4Address
    , _event_details  :: LBS.ByteString
    }
  deriving (Show)

data Source = ACC | AQQ | ERR | QQQ
  deriving (Show,Read)

presentEvent :: Event -> LBS.ByteString
presentEvent Event{..} = LBS.pack $
  printf "%04d %s %s %-7s %3d.%3d.%3d.%3d [%s]"
    (getLineNo                _event_line    )
    (show                     _event_source  )
    (show                     _event_utc     )
    (maybe "-" svrty_kw       _event_severity)
                              a b c d
    (LBS.unpack               _event_details )
  where
    (a,b,c,d) = _event_address

    svrty_kw  = T.unpack . fst . severityKeywords

class IsEvent a where
  mkEvent :: LineNo -> Source -> a -> Event

instance IsEvent Access where
  mkEvent lno src Access{..} =
    Event
      { _event_line     = lno
      , _event_source   = src
      , _event_utc      = _a_time_local
      , _event_severity = Nothing
      , _event_address  = _a_remote_addr
      , _event_details  = LBS.pack $
          printf "%s %d %d %s %s %s"
            (T.unpack _a_request        )
                      _a_status
                      _a_body_bytes
            (T.unpack _a_http_referrer  )
            (T.unpack _a_http_user_agent)
            (T.unpack _a_other          )
      }

instance IsEvent Error where
  mkEvent lno src ERROR{..} =
    Event
      { _event_line     = lno
      , _event_source   = src
      , _event_utc      = UTCTime _e_date $ timeOfDayToTime _e_time
      , _event_severity = Just _e_severity
      , _event_address  = (0,0,0,0)
      , _event_details  = LBS.pack $ printf "%d#%d: %s" pid tid $ LBS.unpack _e_other
      }
    where
      (pid,tid) = _e_pid_tid

instance IsEvent LBS.ByteString where
  mkEvent lno src lbs =
    Event
      { _event_line     = lno
      , _event_source   = src
      , _event_utc      = read "1970-01-01 00:00:00"
      , _event_severity = Nothing
      , _event_address  = (0,0,0,0)
      , _event_details  = lbs
      }


--
-- REOptions and Prelude
--

lpo :: PCRE.REOptions
lpo = PCRE.makeREOptions lp_prelude

lp_prelude :: Macros RE
lp_prelude = runIdentity $ mkMacros mk regexType ExclCaptures lp_env
  where
    mk   = maybe oops Identity . PCRE.compileRegexWithOptions PCRE.noPreludeREOptions

    oops = error "lp_prelude"

lp_macro_table :: String
lp_macro_table = formatMacroTable regexType lp_env

lp_macro_summary :: MacroID -> String
lp_macro_summary = formatMacroSummary regexType lp_env

lp_macro_sources :: String
lp_macro_sources = formatMacroSources regexType ExclCaptures lp_env

lp_macro_source :: MacroID -> String
lp_macro_source = formatMacroSource regexType ExclCaptures lp_env

lp_env :: MacroEnv
lp_env = PCRE.preludeEnv `HML.union` HML.fromList
    [ f "user"        user_macro
    , f "pid#tid:"    pid_tid_macro
    , f "access"      access_macro
    , f "access_deg"  access_deg_macro
    , f "error"       error_macro
    ]
  where
    f mid mk = (mid, mk lp_env mid)


--
-- The Macro Descriptors
--

user_macro :: MacroEnv -> MacroID -> MacroDescriptor
user_macro env mid =
  runTests regexType parse_user samples env mid
    MacroDescriptor
      { macroSource          = "(?:-|[^[:space:]]+)"
      , macroSamples         = map fst samples
      , macroCounterSamples = counter_samples
      , macroTestResults    = []
      , macroParser          = Just "parse_user"
      , macroDescription     = "a user ident (per RFC1413)"
      }
  where
    samples :: [(String,User)]
    samples =
        [ f "joe"
        ]
      where
        f nm = (nm,User $ LBS.pack nm)

    counter_samples =
        [ "joe user"
        ]

pid_tid_macro :: MacroEnv -> MacroID -> MacroDescriptor
pid_tid_macro env mid =
  runTests regexType parse_pid_tid samples env mid
    MacroDescriptor
      { macroSource          = "(?:@{%nat})#(?:@{%nat}):"
      , macroSamples         = map fst samples
      , macroCounterSamples = counter_samples
      , macroTestResults    = []
      , macroParser          = Just "parse_pid_tid"
      , macroDescription     = "<PID>#<TID>:"
      }
  where
    samples :: [(String,(Int,Int))]
    samples =
        [ f "1378#0:" (1378,0)
        ]
      where
        f = (,)

    counter_samples =
        [ ""
        , "24#:"
        , "24.365:"
        ]

access_macro :: MacroEnv -> MacroID -> MacroDescriptor
access_macro env mid =
  runTests' regexType (parse_access . fmap LBS.pack) samples env mid
    MacroDescriptor
      { macroSource          = access_re
      , macroSamples         = map fst samples
      , macroCounterSamples = counter_samples
      , macroTestResults    = []
      , macroParser          = Just "parse_a"
      , macroDescription     = "an Nginx access log file line"
      }
  where
    samples :: [(String,Access)]
    samples =
        [ (,) "192.168.100.200 - - [12/Jan/2016:12:08:36 +0000] \"GET / HTTP/1.1\" 200 3700 \"-\" \"My Agent\" \"-\""
            Access
              { _a_remote_addr      = (192,168,100,200)
              , _a_remote_user      = "-"
              , _a_time_local       = read "2016-01-12 12:08:36 UTC"
              , _a_request          = "GET / HTTP/1.1"
              , _a_status           = 200
              , _a_body_bytes       = 3700
              , _a_http_referrer    = "-"
              , _a_http_user_agent  = "My Agent"
              , _a_other            = "-"
              }
        ]

    counter_samples =
        [ ""
        , " -  [] \"\"   \"\" \"\" \"\""
        ]

access_deg_macro :: MacroEnv -> MacroID -> MacroDescriptor
access_deg_macro env mid =
  runTests' regexType (parse_deg_access . fmap LBS.pack) samples env mid
    MacroDescriptor
      { macroSource          = " -  \\[\\] \"\"   \"\" \"\" \"\""
      , macroSamples         = map fst samples
      , macroCounterSamples = counter_samples
      , macroTestResults    = []
      , macroParser          = Nothing
      , macroDescription     = "a degenerate Nginx access log file line"
      }
  where
    samples :: [(String,Access)]
    samples =
        [ (,) " -  [] \"\"   \"\" \"\" \"\"" deg_access
        ]

    counter_samples =
        [ ""
        , "foo"
        ]

error_macro :: MacroEnv -> MacroID -> MacroDescriptor
error_macro env mid =
  runTests' regexType (parse_error . fmap LBS.pack) samples env mid
    MacroDescriptor
      { macroSource          = error_re
      , macroSamples         = map fst samples
      , macroCounterSamples = counter_samples
      , macroTestResults    = []
      , macroParser          = Just "parse_e"
      , macroDescription     = "an Nginx error log file line"
      }
  where
    samples :: [(String,Error)]
    samples =
        [ (,) "2016/12/21 11:53:35 [emerg] 1378#0: foo"
            ERROR
              { _e_date     = read "2016-12-21"
              , _e_time     = read "11:53:35"
              , _e_severity = Emerg
              , _e_pid_tid  = (1378,0)
              , _e_other    = " foo"
              }
        , (,) "2017/01/04 05:40:19 [error] 31623#0: *1861296 no \"ssl_certificate\" is defined in server listening on SSL port while SSL handshaking, client: 192.168.31.38, server: 0.0.0.0:80"
            ERROR
              { _e_date     = read "2017-01-04"
              , _e_time     = read "05:40:19"
              , _e_severity = Err
              , _e_pid_tid  = (31623,0)
              , _e_other    = " *1861296 no \"ssl_certificate\" is defined in server listening on SSL port while SSL handshaking, client: 192.168.31.38, server: 0.0.0.0:80"
              }
        ]

    counter_samples =
        [ ""
        , "foo"
        ]
\end{code}

\begin{code}

--
-- Access, access_re, deg_access, parse_deg_access, parse_access
--

data Access =
  Access
    { _a_remote_addr      :: !IPV4Address
    , _a_remote_user      :: !User
    , _a_time_local       :: !UTCTime
    , _a_request          :: !T.Text
    , _a_status           :: !Int
    , _a_body_bytes       :: !Int
    , _a_http_referrer    :: !T.Text
    , _a_http_user_agent  :: !T.Text
    , _a_other            :: !T.Text
    }
  deriving (Eq,Show)
\end{code}

\begin{code}
access_re :: RegexSource
access_re = RegexSource $ unwords
  [ "(@{%address.ipv4})"
  , "-"
  , "(@{user})"
  , "\\[(@{%datetime.clf})\\]"
  , "(@{%string.simple})"
  , "(@{%nat})"
  , "(@{%nat})"
  , "(@{%string.simple})"
  , "(@{%string.simple})"
  , "(@{%string.simple})"
  ]
\end{code}

\begin{code}
deg_access :: Access
deg_access =
  Access
    { _a_remote_addr      = (0,0,0,0)
    , _a_remote_user      = "-"
    , _a_time_local       = read "1970-01-01 00:00:00"
    , _a_request          = ""
    , _a_status           = 0
    , _a_body_bytes       = 0
    , _a_http_referrer    = ""
    , _a_http_user_agent  = ""
    , _a_other            = ""
    }

parse_deg_access :: Match LBS.ByteString -> Maybe Access
parse_deg_access Match{..} =
  case matchSource == " -  [] \"\"   \"\" \"\" \"\"" of
    True  -> Just deg_access
    False -> Nothing

parse_a :: LBS.ByteString -> Maybe Access
parse_a lbs = parse_access $ lbs ?=~ [re_|@{access}|] lpo

parse_access :: Match LBS.ByteString -> Maybe Access
parse_access cs =
  Access
    <$> f parseIPv4Address  [cp|1|]
    <*> f parse_user        [cp|2|]
    <*> f parseDateTimeCLF  [cp|3|]
    <*> f parseSimpleString [cp|4|]
    <*> f parseInteger      [cp|5|]
    <*> f parseInteger      [cp|6|]
    <*> f parseSimpleString [cp|7|]
    <*> f parseSimpleString [cp|8|]
    <*> f parseSimpleString [cp|9|]
  where
    f psr i = psr $ capturedText $ capture i cs


--
-- Error, error_re, parse_error
--

data Error =
  ERROR
    { _e_date     :: Day
    , _e_time     :: TimeOfDay
    , _e_severity :: Severity
    , _e_pid_tid  :: (Int,Int)
    , _e_other    :: LBS.ByteString
    }
  deriving (Eq,Show)

error_re :: RegexSource
error_re = RegexSource $ unwords
  [ "(@{%date.slashes})"
  , "(@{%time})"
  , "\\[(@{%syslog.severity})\\]"
  , "(@{pid#tid:})(.*)"
  ]

parse_e :: LBS.ByteString -> Maybe Error
parse_e lbs = parse_error $ lbs ?=~ [re_|@{error}|] lpo

parse_error :: Match LBS.ByteString -> Maybe Error
parse_error cs =
  ERROR
    <$> f parseSlashesDate    [cp|1|]
    <*> f parseTimeOfDay      [cp|2|]
    <*> f parseSeverity [cp|3|]
    <*> f parse_pid_tid       [cp|4|]
    <*> f Just                [cp|5|]
  where
    f psr i = psr $ capturedText $ capture i cs


--
-- User, parseUser
--

newtype User =
    User { _User :: LBS.ByteString }
  deriving (IsString,Ord,Eq,Show)

parse_user :: Replace a => a -> Maybe User
parse_user = Just . User . LBS.pack . unpackR


--
-- parse_pid_tid
--

parse_pid_tid :: Replace a => a -> Maybe (Int,Int)
parse_pid_tid x = case allMatches $ unpackR x S.*=~ [re|@{%nat}|] of
    [cs,cs'] -> (,) <$> p cs <*> p cs'
    _        -> Nothing
  where
    p cs = matchCapture cs >>= parseInteger . capturedText

regexType :: RegexType
regexType = PCRE.regexType
\end{code}
