\begin{code}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Main (main) where

import           Control.Applicative
import           Data.Array
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy.Char8     as LBS
import           Data.Foldable
import           Data.Monoid
import qualified Data.Sequence                  as S
import           Data.String
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as LT
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.RE
import           Text.RE.Internal.NamedCaptures
import qualified Text.RE.PCRE           as PCRE
import           Text.RE.TDFA           as TDFA
import           Text.RE.TestBench

import           Text.RE.TDFA.String()


main :: IO ()
main = defaultMain $
  testGroup "Tests"
    [ prelude_tests
    , parsing_tests
    , core_tests
    , replace_tests
    , options_tests
    , namedCapturesTestTree
    ]

prelude_tests :: TestTree
prelude_tests = testGroup "Prelude"
  [ tc TDFA TDFA.preludeEnv
  , tc PCRE PCRE.preludeEnv
  ]
  where
    tc rty m_env =
      testCase (show rty) $ do
        dumpMacroTable "prelude" rty m_env
        assertBool "testMacroEnv" =<< testMacroEnv "prelude" rty m_env

str, str' :: String
str       = "a bbbb aa b"
str'      = "foo"

regex_, regex_alt :: RE
regex_    = [re|(a+) (b+)|]
regex_alt = [re|(a+)|(b+)|]

regex_str_matches :: Matches String
regex_str_matches =
  Matches
    { matchesSource = "a bbbb aa b"
    , allMatches =
        [ regex_str_match
        , regex_str_match_2
        ]
    }

regex_str_match :: Match String
regex_str_match =
  Match
    { matchSource   = "a bbbb aa b"
    , captureNames  = noCaptureNames
    , matchArray    = array (0,2)
        [ (0,Capture {captureSource = "a bbbb aa b", capturedText = "a bbbb", captureOffset = 0, captureLength = 6})
        , (1,Capture {captureSource = "a bbbb aa b", capturedText = "a"     , captureOffset = 0, captureLength = 1})
        , (2,Capture {captureSource = "a bbbb aa b", capturedText = "bbbb"  , captureOffset = 2, captureLength = 4})
        ]
    }

regex_str_match_2 :: Match String
regex_str_match_2 =
  Match
    { matchSource   = "a bbbb aa b"
    , captureNames  = noCaptureNames
    , matchArray    = array (0,2)
        [ (0,Capture {captureSource = "a bbbb aa b", capturedText = "aa b", captureOffset = 7 , captureLength = 4})
        , (1,Capture {captureSource = "a bbbb aa b", capturedText = "aa"  , captureOffset = 7 , captureLength = 2})
        , (2,Capture {captureSource = "a bbbb aa b", capturedText = "b"   , captureOffset = 10, captureLength = 1})
        ]
    }

regex_alt_str_matches :: Matches String
regex_alt_str_matches =
  Matches
    { matchesSource = "a bbbb aa b"
    , allMatches    =
        [ Match
            { matchSource   = "a bbbb aa b"
            , captureNames  = noCaptureNames
            , matchArray    = array (0,2)
                [ (0,Capture {captureSource = "a bbbb aa b", capturedText = "a", captureOffset = 0, captureLength = 1})
                , (1,Capture {captureSource = "a bbbb aa b", capturedText = "a", captureOffset = 0, captureLength = 1})
                , (2,Capture {captureSource = "a bbbb aa b", capturedText = "", captureOffset = -1, captureLength = 0})
                ]
            }
        , Match
            { matchSource   = "a bbbb aa b"
            , captureNames  = noCaptureNames
            , matchArray    = array (0,2)
                [ (0,Capture {captureSource = "a bbbb aa b", capturedText = "bbbb", captureOffset = 2 , captureLength = 4})
                , (1,Capture {captureSource = "a bbbb aa b", capturedText = ""    , captureOffset = -1, captureLength = 0})
                , (2,Capture {captureSource = "a bbbb aa b", capturedText = "bbbb", captureOffset = 2 , captureLength = 4})
                ]
            }
        , Match
            { matchSource   = "a bbbb aa b"
            , captureNames  = noCaptureNames
            , matchArray    = array (0,2)
                [ (0,Capture {captureSource = "a bbbb aa b", capturedText = "aa", captureOffset = 7 , captureLength = 2})
                , (1,Capture {captureSource = "a bbbb aa b", capturedText = "aa", captureOffset = 7 , captureLength = 2})
                , (2,Capture {captureSource = "a bbbb aa b", capturedText = ""  , captureOffset = -1, captureLength = 0})
                ]
            }
        , Match
            { matchSource   = "a bbbb aa b"
            , captureNames  = noCaptureNames
            , matchArray    = array (0,2)
                [ (0,Capture {captureSource = "a bbbb aa b", capturedText = "b", captureOffset = 10, captureLength = 1})
                , (1,Capture {captureSource = "a bbbb aa b", capturedText = "" , captureOffset = -1, captureLength = 0})
                , (2,Capture {captureSource = "a bbbb aa b", capturedText = "b", captureOffset = 10, captureLength = 1})
                ]
            }
        ]
    }

parsing_tests :: TestTree
parsing_tests = testGroup "Parsing"
  [ testCase "complete check (matchM/ByteString)" $ do
      r    <- compileRegex () $ reSource regex_
      assertEqual "Match" (B.pack <$> regex_str_match) $ B.pack str ?=~ r
  , testCase "matched (matchM/Text)" $ do
      r     <- compileRegex () $ reSource regex_
      assertEqual "matched" True $ matched $ T.pack str ?=~ r
  ]

core_tests :: TestTree
core_tests = testGroup "Match"
  [ testCase "text (=~~Text.Lazy)" $ do
      txt <- LT.pack str =~~ [re|(a+) (b+)|] :: IO (LT.Text)
      assertEqual "text" txt "a bbbb"
  , testCase "multi (=~~/String)" $ do
      let sm = str =~ regex_ :: Match String
          m  = capture [cp|0|] sm
      assertEqual "captureSource" "a bbbb aa b" $ captureSource m
      assertEqual "capturedText"  "a bbbb"      $ capturedText  m
      assertEqual "capturePrefix" ""            $ capturePrefix m
      assertEqual "captureSuffix" " aa b"       $ captureSuffix m
  , testCase "complete (=~~/ByteString)" $ do
      mtch <- B.pack str =~~ regex_ :: IO (Match B.ByteString)
      assertEqual "Match" mtch $ B.pack <$> regex_str_match
  , testCase "complete (all,String)" $ do
      let mtchs = str =~ regex_     :: Matches String
      assertEqual "Matches" mtchs regex_str_matches
  , testCase "complete (all,reg_alt)" $ do
      let mtchs = str =~ regex_alt  :: Matches String
      assertEqual "Matches" mtchs regex_alt_str_matches
  , testCase "complete (=~~,all)" $ do
      mtchs <- str =~~ regex_       :: IO (Matches String)
      assertEqual "Matches" mtchs regex_str_matches
  , testCase "fail (all)" $ do
      let mtchs = str' =~ regex_    :: Matches String
      assertEqual "not.anyMatches" False $ anyMatches mtchs
  ]

replace_tests :: TestTree
replace_tests = testGroup "Replace"
  [ testCase "String/single" $ do
      let m = str =~ regex_ :: Match String
          r = replaceCaptures' ALL fmt m
      assertEqual "replaceCaptures'" r "(0:0:(0:1:a) (0:2:bbbb)) aa b"
  , testCase "String/alt" $ do
      let ms = str =~ regex_ :: Matches String
          r  = replaceAllCaptures' ALL fmt ms
      chk r
  , testCase "String" $ do
      let ms = str =~ regex_ :: Matches String
          r  = replaceAllCaptures' ALL fmt ms
      chk r
  , testCase "ByteString" $ do
      let ms = B.pack str =~ regex_ :: Matches B.ByteString
          r  = replaceAllCaptures' ALL fmt ms
      chk r
  , testCase "LBS.ByteString" $ do
      let ms = LBS.pack str =~ regex_ :: Matches LBS.ByteString
          r  = replaceAllCaptures' ALL fmt ms
      chk r
  , testCase "Seq Char" $ do
      let ms = S.fromList str =~ regex_ :: Matches (S.Seq Char)
          f  = \_ (Location i j) Capture{..} -> Just $ S.fromList $
                  "(" <> show i <> ":" <> show_co j <> ":" <>
                    toList capturedText <> ")"
          r  = replaceAllCaptures' ALL f ms
      assertEqual "replaceAllCaptures'" r $
        S.fromList "(0:0:(0:1:a) (0:2:bbbb)) (1:0:(1:1:aa) (1:2:b))"
  , testCase "Text" $ do
      let ms = T.pack str =~ regex_ :: Matches T.Text
          r  = replaceAllCaptures' ALL fmt ms
      chk r
  , testCase "LT.Text" $ do
      let ms = LT.pack str =~ regex_ :: Matches LT.Text
          r  = replaceAllCaptures' ALL fmt ms
      chk r
  ]
  where
    chk r =
      assertEqual
        "replaceAllCaptures'"
        r
        "(0:0:(0:1:a) (0:2:bbbb)) (1:0:(1:1:aa) (1:2:b))"

    fmt :: (IsString s,Replace s) => a -> Location -> Capture s -> Maybe s
    fmt _ (Location i j) Capture{..} = Just $ "(" <> pack_ (show i) <> ":" <>
      pack_ (show_co j) <> ":" <> capturedText <> ")"

    show_co (CaptureOrdinal j) = show j

options_tests :: TestTree
options_tests = testGroup "Simple Options"
  [ testGroup "TDFA Simple Options"
      [ testCase "default (MultilineSensitive)" $ assertEqual "#" 2 $
          countMatches $ s TDFA.*=~ [TDFA.re|[0-9a-f]{2}$|]
      , testCase "MultilineSensitive" $ assertEqual "#" 2 $
          countMatches $ s TDFA.*=~ [TDFA.reMultilineSensitive|[0-9a-f]{2}$|]
      , testCase "MultilineInsensitive" $ assertEqual "#" 4 $
          countMatches $ s TDFA.*=~ [TDFA.reMultilineInsensitive|[0-9a-f]{2}$|]
      , testCase "BlockSensitive" $ assertEqual "#" 0 $
          countMatches $ s TDFA.*=~ [TDFA.reBlockSensitive|[0-9a-f]{2}$|]
      , testCase "BlockInsensitive" $ assertEqual "#" 1 $
          countMatches $ s TDFA.*=~ [TDFA.reBlockInsensitive|[0-9a-f]{2}$|]
      ]
  , testGroup "PCRE Simple Options"
      [ testCase "default (MultilineSensitive)" $ assertEqual "#" 2 $
          countMatches $ s PCRE.*=~ [PCRE.re|[0-9a-f]{2}$|]
      , testCase "MultilineSensitive" $ assertEqual "#" 2 $
          countMatches $ s PCRE.*=~ [PCRE.reMultilineSensitive|[0-9a-f]{2}$|]
      , testCase "MultilineInsensitive" $ assertEqual "#" 4 $
          countMatches $ s PCRE.*=~ [PCRE.reMultilineInsensitive|[0-9a-f]{2}$|]
      , testCase "BlockSensitive" $ assertEqual "#" 0 $
          countMatches $ s PCRE.*=~ [PCRE.reBlockSensitive|[0-9a-f]{2}$|]
      , testCase "BlockInsensitive" $ assertEqual "#" 1 $
          countMatches $ s PCRE.*=~ [PCRE.reBlockInsensitive|[0-9a-f]{2}$|]
      ]
    ]
  where
    s = "0a\nbb\nFe\nA5" :: String
\end{code}
