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
  , Test(..)
  , runTheTests
  , checkThis
  , checkThisWith
  , convertMaybeTextList
  , castInt
  , packLBS
  , test_pp
  , include
  , cmp
  , dumpMacroTable
  , sortImports
  , groupSort
  , groupSortBy
  , read_file
  , write_file
  ) where

import           Control.Applicative
import           Control.Exception
import qualified Control.Monad                            as M
import qualified Data.ByteString.Lazy.Char8               as LBS
import qualified Data.List                                as L
import           Data.Maybe
import qualified Data.Text                                as T
import           Prelude.Compat
import qualified Shelly                                   as SH
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Printf
import           Text.RE.Replace
import           Text.RE.TDFA
import           Text.RE.TestBench
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
    nada <- null . linesMatched LinesMatched <$> grepLines rex' "lib/md/roadmap-incl.md"
    M.when nada $
      error $ vrn_s ++ ": not ticked off in the roadmap"
    rex  <- compileRegex $ printf "%d\\.%d\\.%d\\.%d" _vrn_a _vrn_b _vrn_c _vrn_d
    nope <- null . linesMatched LinesMatched <$> grepLines rex "changelog"
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
    flip replaceAll ms . packR . presentVrn <$> readCurrentVersion
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

runTheTests :: [Test] -> IO ()
runTheTests tests = do
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
checkThis = checkThisWith id

checkThisWith :: (Show a,Eq a) => (b->a) -> String -> b -> a -> Test
checkThisWith f lab ref0 val =
  Test
    { testLabel    = lab
    , testExpected = show ref
    , testResult   = show val
    , testPassed   = ref == val
    }
  where
    ref = f ref0

convertMaybeTextList :: [Maybe String] -> [Maybe T.Text]
convertMaybeTextList = map $ fmap T.pack

castInt :: Int -> Int
castInt = id

packLBS :: String -> LBS.ByteString
packLBS = LBS.pack

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
-- | this function looks for lines of the form
--
--    `%include <file> [exclude <RE>]`
--
-- and replaces them with the contents of the named file, optionally
-- excluding any lines that match the given RE.
include :: LBS.ByteString -> IO LBS.ByteString
include = sed' $ Select
    [ Function [re|^%include ${file}(@{%string})$|]                              TOP incl
    , Function [re|^%include ${file}(@{%string}) *exclude *${rex}(@{%string})$|] TOP incl
    , Function [re|^.*$|]                                                        TOP nop
    ]
  where
    incl _ mtch _ _ = include' mtch
    nop  _ _    _ _ = return Nothing

-- | processes the match from a '%include' line, analyses the match,
-- fetches the file, optionally excludes lines specified by an RE,
-- returning the text to include.
include' :: Match LBS.ByteString -> IO (Maybe LBS.ByteString)
include' mtch = do
    ftr <- case prs_s <$> mtch !$$? [cp|rex|] of
      Nothing     -> return id
      Just re_lbs -> excl <$> makeRegex re_lbs
    Just . ftr <$> LBS.readFile (prs_s $ mtch !$$ [cp|file|])
  where
    excl :: RE -> LBS.ByteString -> LBS.ByteString
    excl rex =
        LBS.unlines . map (matchesSource . getLineMatches)
          . filter (not . anyMatches . getLineMatches)
          . grepFilter rex

    prs_s  = maybe (error "include'") T.unpack . parseString
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


dumpMacroTable
--------------

\begin{code}
-- | dump a MacroEnv into the docs directory
dumpMacroTable :: FilePath
               -> FilePath
               -> RegexType
               -> MacroEnv
               -> IO ()
dumpMacroTable fp_t fp_s rty m_env = do
  writeFile fp_t $ formatMacroTable   rty              m_env
  writeFile fp_s $ formatMacroSources rty ExclCaptures m_env
\end{code}


sortImports
-----------

\begin{code}
sortImports :: LBS.ByteString -> LBS.ByteString
sortImports lbs =
    LBS.unlines $ map (matchesSource . getLineMatches) $
      hdr ++ L.sortBy cMp bdy
  where
    cMp ln1 ln2 = case (extr ln1,extr ln2) of
        (Nothing,Nothing) -> EQ
        (Nothing,Just _ ) -> GT
        (Just _ ,Nothing) -> LT
        (Just x ,Just  y) -> compare x y

    extr Line{..} = case allMatches getLineMatches of
      mtch:_  -> mtch !$$? [cp|mod|]
      _       -> Nothing

    (hdr,bdy) = span (not . anyMatches . getLineMatches) lns
    lns       = grepFilter rex lbs
    rex       = [re|^import +(qualified|         ) ${mod}([^ ].*)$|]
\end{code}



groupSort and groupSortBy
-------------------------

\begin{code}
-- | Sort a list of elements with a stable sort, grouping together the
-- equal elements with the argument grouping function
groupSort :: (Ord a) => (a->[a]->b) -> [a] -> [b]
groupSort = groupSortBy compare

-- | Sort a list of elements with a stable sort, grouping together the
-- equal elements with the argument grouping function.
groupSortBy :: (a->a->Ordering)
            -> (a->[a]->b)
            -> [a]
            -> [b]
groupSortBy comp grp = aggregate . L.sortBy comp
  where
    aggregate []    = []
    aggregate (h:t) = seq g $ g : aggregate rst
      where
        g         = grp h eqs
        (eqs,rst) = span is_le t

        is_le x   = case comp x h of
          LT -> True
          EQ -> True
          GT -> False
\end{code}


read_file and write_file
------------------------

\begin{code}
read_file :: FilePath -> IO LBS.ByteString
read_file "-" = LBS.getContents
read_file fp  = LBS.readFile fp

write_file :: FilePath -> LBS.ByteString ->IO ()
write_file "-" = LBS.putStr
write_file fp  = LBS.writeFile fp
\end{code}
