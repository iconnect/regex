\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module TestKit
  ( Vrn(..)
  , presentVrn
  , parseVrn
  , bumpVersion
  , substVersion
  , substVersion_
  , readCurrentVersion
  , Test
  , runTests
  , checkThis
  , test_pp
  , include
  , cmp
  ) where

import           Control.Applicative
import           Control.Exception
import qualified Control.Monad                            as M
import           Data.Maybe
import qualified Data.Text                                as T
import qualified Data.ByteString.Lazy.Char8               as LBS
import           Prelude.Compat
import qualified Shelly                                   as SH
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Printf
import           Text.RE.Types.IsRegex
import           Text.RE.TDFA
import           Text.RE.TestBench.Parsers
import           Text.RE.Tools.Grep
import           Text.RE.Tools.Sed
\end{code}


Vrn and friends
---------------

\begin{code}
data Vrn = Vrn { _vrn_a, _vrn_b, _vrn_c, _vrn_d :: Int }
  deriving (Show,Eq,Ord)

presentVrn :: Vrn -> String
presentVrn Vrn{..} = printf "%d.%d.%d.%d" _vrn_a _vrn_b _vrn_c _vrn_d

parseVrn :: String -> Vrn
parseVrn vrn_s = case matched m of
    True  -> Vrn (p [cp|a|]) (p [cp|b|]) (p [cp|c|]) (p [cp|d|])
    False -> error $ "not a valid version: " ++ vrn_s
  where
    p c  = fromMaybe oops $ parseInteger $ m !$$ c
    m    = vrn_s ?=~ [re|^${a}(@{%nat})\.${b}(@{%nat})\.${c}(@{%nat})\.${d}(@{%nat})$|]

    oops = error "parseVrn"

-- | register a new version of the package
bumpVersion :: String -> IO ()
bumpVersion vrn_s = do
    vrn0 <- readCurrentVersion
    rex' <- compileRegex $ printf "- \\[[xX]\\].*%d\\.%d\\.%d\\.%d" _vrn_a _vrn_b _vrn_c _vrn_d
    nada <- null . linesMatched <$> grepLines rex' "lib/md/roadmap-incl.md"
    M.when nada $
      error $ vrn_s ++ ": not ticked off in the roadmap"
    rex  <- compileRegex $ printf "%d\\.%d\\.%d\\.%d" _vrn_a _vrn_b _vrn_c _vrn_d
    nope <- null . linesMatched <$> grepLines rex "changelog"
    M.when nope $
      error $ vrn_s ++ ": not in the changelog"
    case vrn > vrn0 of
      True  -> do
        write_current_version vrn
        substVersion "lib/hackage-template.svg" "docs/badges/hackage.svg"
      False -> error $
        printf "version not later ~(%s > %s)" vrn_s $ presentVrn vrn0
  where
    vrn@Vrn{..} = parseVrn vrn_s

substVersion :: FilePath -> FilePath -> IO ()
substVersion in_f out_f =
    LBS.readFile in_f >>= substVersion_ >>= LBS.writeFile out_f

substVersion_ :: (IsRegex RE a,Replace a) => a -> IO a
substVersion_ txt =
    flip replaceAll ms . packE . presentVrn <$> readCurrentVersion
  where
    ms = txt *=~ [re|<<\$version\$>>|]

readCurrentVersion :: IO Vrn
readCurrentVersion = parseVrn <$> readFile "lib/version.txt"

write_current_version :: Vrn -> IO ()
write_current_version = writeFile "lib/version.txt" . presentVrn
\end{code}


Test and friends
----------------

\begin{code}
data Test =
  Test
    { testLabel    :: String
    , testExpected :: String
    , testResult   :: String
    , testPassed   :: Bool
    }
  deriving (Show)

runTests :: [Test] -> IO ()
runTests tests = do
  as <- getArgs
  case as of
    [] -> return ()
    _  -> do
      pn <- getProgName
      putStrLn $ "usage:\n  "++pn++" --help"
      exitWith $ ExitFailure 1
  case filter (not . testPassed) tests of
    []  -> putStrLn $ "All "++show (length tests)++" tests passed."
    fts -> do
      mapM_ (putStr . present_test) fts
      putStrLn $ show (length fts) ++ " tests failed."
      exitWith $ ExitFailure 1

checkThis :: (Show a,Eq a) => String -> a -> a -> Test
checkThis lab ref val =
  Test
    { testLabel    = lab
    , testExpected = show ref
    , testResult   = show val
    , testPassed   = ref == val
    }

present_test :: Test -> String
present_test Test{..} = unlines
  [ "test: " ++ testLabel
  , "  expected : " ++ testExpected
  , "  result   : " ++ testResult
  , "  passed   : " ++ (if testPassed then "passed" else "**FAILED**")
  ]
\end{code}

\begin{code}
test_pp :: String
        -> (FilePath->FilePath->IO())
        -> FilePath
        -> FilePath
        -> IO ()
test_pp lab loop test_file gold_file = do
    createDirectoryIfMissing False "tmp"
    loop test_file tmp_pth
    ok <- cmp (T.pack tmp_pth) (T.pack gold_file)
    case ok of
      True  -> return ()
      False -> do
        putStrLn $ lab ++ ": mismatch with " ++ gold_file
        exitWith $ ExitFailure 1
  where
    tmp_pth = "tmp/mod.lhs"
\end{code}


simple include processor
------------------------

\begin{code}
include :: LBS.ByteString -> IO LBS.ByteString
include = sed' $ Select
    [ (,) [re|^%include ${file}(@{%string})$|] $ Function TOP   incl
    , (,) [re|^.*$|]                           $ Function TOP $ \_ _ _ _->return Nothing
    ]
  where
    incl _ mtch _ _ = Just <$> LBS.readFile (prs_s $ mtch !$$ [cp|file|])
    prs_s           = maybe (error "include") T.unpack . parseString
\end{code}

cmp
---

\begin{code}
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
