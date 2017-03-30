Regex Module Gen
================

All of the modules that make up the API are generated from the
`Text.RE.TDFA.ByteString.Lazy` module using this script.

The tool is self-testing: run it with no arguments (or `cabal test`).


\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE CPP                        #-}

module Main (main) where

import           Control.Exception
import qualified Data.ByteString.Lazy.Char8               as LBS
import qualified Data.Text                                as T
import           Prelude.Compat
import qualified Shelly                                   as SH
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           Text.RE.TDFA.ByteString.Lazy
import           Text.RE.Tools.Sed
import           Text.RE.ZeInternals.Types.SearchReplace


type ModPath = String


main :: IO ()
main = do
  (pn,as) <- (,) <$> getProgName <*> getArgs
  case as of
    []        -> test
    ["test"]  -> test
    ["gen"]   -> gen
    _         -> do
      hPutStrLn stderr $ "usage: " ++ pn ++ " [test|gen]"
      exitWith $ ExitFailure 1


------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------

test :: IO ()
test = do
  createDirectoryIfMissing False "tmp"
  tdfa_ap_ok <- and <$> mapM (test' source_api_mp) tdfa_api_edits
  pcre_ap_ok <- and <$> mapM (test' source_api_mp) pcre_api_edits
  case tdfa_ap_ok && pcre_ap_ok of
    True  -> return ()
    False -> exitWith $ ExitFailure 1

type SedScript = Edits IO RE LBS.ByteString

test' :: ModPath -> (ModPath,SedScript) -> IO Bool
test' src_mp (mp,scr) = do
    putStrLn mp
    tp <- is_text_present
    sed scr (mod_filepath tp src_mp) tmp_pth
    cmp     (T.pack tmp_pth) (T.pack $ mod_filepath tp mp)
  where
    tmp_pth = "tmp/prog.hs"


------------------------------------------------------------------------
-- Generating
------------------------------------------------------------------------

gen :: IO ()
gen = do
  mapM_ (gen' source_api_mp) tdfa_api_edits
  mapM_ (gen' source_api_mp) pcre_api_edits
  mapM_ (gen' source_ed_mp ) tdfa_ed_edits
  mapM_ (gen' source_ed_mp ) pcre_ed_edits

gen' :: FilePath -> (ModPath,SedScript) -> IO ()
gen' src_mp (mp,scr) = do
  putStrLn mp
  tp <- is_text_present
  sed scr (mod_filepath tp src_mp) (mod_filepath tp mp)


------------------------------------------------------------------------
-- The API edits
------------------------------------------------------------------------

tdfa_api_edits :: [(ModPath,SedScript)]
tdfa_api_edits =
  [ tdfa_api_edit "TDFA.ByteString"       "B.ByteString"    "import qualified Data.ByteString               as B"
  , tdfa_api_edit "TDFA.Sequence"         "(S.Seq Char)"    "import qualified Data.Sequence                 as S"
  , tdfa_api_edit "TDFA.String"           "String"          ""
  , tdfa_api_edit "TDFA.Text"             "T.Text"          "import qualified Data.Text                     as T"
  , tdfa_api_edit "TDFA.Text.Lazy"        "TL.Text"         "import qualified Data.Text.Lazy                as TL"
  ]

pcre_api_edits :: [(ModPath,SedScript)]
pcre_api_edits =
  [ pcre_api_edit "PCRE.ByteString"       "B.ByteString"    "import qualified Data.ByteString               as B"
  , pcre_api_edit "PCRE.ByteString.Lazy"  "LBS.ByteString"  "import qualified Data.ByteString.Lazy          as LBS"
  , pcre_api_edit "PCRE.Sequence"         "(S.Seq Char)"    "import qualified Data.Sequence                 as S"
  , pcre_api_edit "PCRE.String"           "String"          ""
  ]

tdfa_api_edit :: ModPath
              -> LBS.ByteString
              -> LBS.ByteString
              -> (ModPath,SedScript)
tdfa_api_edit mp bs_lbs import_lbs =
    (,) fmp $ Pipe
        [ Template $ SearchReplace module_re $ LBS.pack mp
        , Template $ SearchReplace import_re   import_lbs
        , Template $ SearchReplace bs_re       bs_lbs
        ]
  where
    fmp = "Text.RE." ++ mp

pcre_api_edit :: ModPath
              -> LBS.ByteString
              -> LBS.ByteString
              -> (ModPath,SedScript)
pcre_api_edit mp bs_lbs import_lbs =
    (,) fmp $ Pipe
        [ Template $ SearchReplace tdfa_re     "PCRE"
        , Template $ SearchReplace module_re $ LBS.pack mp
        , Template $ SearchReplace import_re   import_lbs
        , Template $ SearchReplace bs_re       bs_lbs
        ]
  where
    fmp = "Text.RE." ++ mp

source_api_mp :: ModPath
source_api_mp = "Text.RE.TDFA.ByteString.Lazy"

tdfa_re, module_re, import_re, bs_re :: RE
tdfa_re   = [re|TDFA|]
module_re = [re|TDFA.ByteString.Lazy|]
import_re = [re|import qualified Data.ByteString.Lazy.Char8 *as LBS|]
bs_re     = [re|LBS.ByteString|]


------------------------------------------------------------------------
-- The ed quasi quoter edits
------------------------------------------------------------------------

source_ed_mp :: ModPath
source_ed_mp = "Text.RE.ZeInternals.SearchReplace.TDFA.ByteString.Lazy"

tdfa_ed_edits :: [(ModPath,SedScript)]
tdfa_ed_edits =
  [ (,) "Text.RE.ZeInternals.SearchReplace.TDFA.ByteString" $ Pipe
      [ Template [ed|SearchReplace.TDFA.ByteString.Lazy///SearchReplace.TDFA.ByteString|]
      , Template [ed|Data.ByteString.Lazy.Char8    as LBS///Data.ByteString.Char8         as B|]
      , Template [ed|LBS.ByteString///B.ByteString|]
      ]
  , (,) "Text.RE.ZeInternals.SearchReplace.TDFA.Sequence" $ Pipe
      [ Template [ed|SearchReplace.TDFA.ByteString.Lazy///SearchReplace.TDFA.Sequence|]
      , Template [ed|Data.ByteString.Lazy.Char8    as LBS///Data.Sequence                 as S|]
      , Template [ed|LBS.ByteString///(S.Seq Char)|]
      ]
  , (,) "Text.RE.ZeInternals.SearchReplace.TDFA.String" $ Pipe
      [ Template [ed|SearchReplace.TDFA.ByteString.Lazy///SearchReplace.TDFA.String|]
      , Template [ed|import qualified Data.ByteString.Lazy.Char8    as LBS///|]
      , Template [ed|LBS.ByteString///String|]
      ]
  , (,) "Text.RE.ZeInternals.SearchReplace.TDFA.Text" $ Pipe
      [ Template [ed|SearchReplace.TDFA.ByteString.Lazy///SearchReplace.TDFA.Text|]
      , Template [ed|Data.ByteString.Lazy.Char8    as LBS///Data.Text                     as T|]
      , Template [ed|LBS.ByteString///T.Text|]
      ]
  , (,) "Text.RE.ZeInternals.SearchReplace.TDFA.Text.Lazy" $ Pipe
      [ Template [ed|SearchReplace.TDFA.ByteString.Lazy///SearchReplace.TDFA.Text.Lazy|]
      , Template [ed|Data.ByteString.Lazy.Char8    as LBS///Data.Text.Lazy                as TL|]
      , Template [ed|LBS.ByteString///TL.Text|]
      ]
  ]

pcre_ed_edits :: [(ModPath,SedScript)]
pcre_ed_edits =
  [ (,) "Text.RE.ZeInternals.SearchReplace.PCRE.ByteString" $ pipe
      [ Template [ed|SearchReplace.TDFA.ByteString.Lazy///SearchReplace.PCRE.ByteString|]
      , Template [ed|Data.ByteString.Lazy.Char8    as LBS///Data.ByteString.Char8         as B|]
      , Template [ed|LBS.ByteString///B.ByteString|]
      ]
  , (,) "Text.RE.ZeInternals.SearchReplace.PCRE.ByteString.Lazy" $ pipe
      [ Template [ed|SearchReplace.TDFA.ByteString.Lazy///SearchReplace.PCRE.ByteString.Lazy|]
      , Template [ed|Text.RE.ZeInternals.TDFA///Text.RE.ZeInternals.PCRE|]
      ]
  , (,) "Text.RE.ZeInternals.SearchReplace.PCRE.Sequence" $ pipe
      [ Template [ed|SearchReplace.TDFA.ByteString.Lazy///SearchReplace.PCRE.Sequence|]
      , Template [ed|Data.ByteString.Lazy.Char8    as LBS///Data.Sequence                 as S|]
      , Template [ed|LBS.ByteString///(S.Seq Char)|]
      , Template [ed|Text.RE.ZeInternals.TDFA///Text.RE.ZeInternals.PCRE|]
      ]
  , (,) "Text.RE.ZeInternals.SearchReplace.PCRE.String" $ pipe
      [ Template [ed|SearchReplace.TDFA.ByteString.Lazy///SearchReplace.PCRE.String|]
      , Template [ed|import qualified Data.ByteString.Lazy.Char8    as LBS///|]
      , Template [ed|LBS.ByteString///String|]
      , Template [ed|Text.RE.ZeInternals.TDFA///Text.RE.ZeInternals.PCRE|]
      ]
  ]
  where
    pipe as = Pipe $ as ++
      [ Template [ed|Text.RE.ZeInternals.TDFA///Text.RE.ZeInternals.PCRE|]
      , Template [ed|Text.RE.ZeInternals.SearchReplace.TDFAEdPrime///Text.RE.ZeInternals.SearchReplace.PCREEdPrime|]
      ]


------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

mod_filepath :: Bool -> ModPath -> FilePath
mod_filepath text_present mp = pfx ++ map tr mp ++ ".hs"
  where
    pfx = case text_present of
      True  -> ""
      False -> "src/"

    tr '.' = '/'
    tr c   = c

is_text_present :: IO Bool
is_text_present = doesDirectoryExist "Text"

cmp :: T.Text -> T.Text -> IO Bool
cmp src dst = handle hdl $ do
    _ <- SH.shelly $ SH.verbosely $
        SH.run "cmp" [src,dst]
    return True
  where
    hdl :: SomeException -> IO Bool
    hdl se = do
      hPutStrLn stderr $
        "testing results against model answers failed: " ++ show se
      return False
\end{code}
