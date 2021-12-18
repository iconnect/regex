Regex Cabal Gen
===============

This tool generates the cabal files for the regex and regex-examples
packages as well as the cabal file for the development tree
(contaiing the combined targets of both packages). In addition it
contains scripts for bumping the version number and generating the
Hackage releases.

The tool is self-testing: run it with no arguments (or `cabal test`).


\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE CPP                        #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8               as LBS
import           Data.Char
import           Data.IORef
import qualified Data.List                                as L
import qualified Data.Map                                 as Map
import           Data.Maybe
import qualified Data.Monoid                              as M
import qualified Data.Text                                as T
import qualified Data.Text.Encoding                       as TE
import           Prelude.Compat
import qualified Shelly                                   as SH
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           TestKit
import           Text.Printf
import           Text.RE.Replace
import           Text.RE.TDFA.ByteString.Lazy
import qualified Text.RE.TDFA.Text                        as T
import           Text.RE.Tools.Grep
import           Text.RE.Tools.Sed


main :: IO ()
main = do
  (pn,as) <- (,) <$> getProgName <*> getArgs
  case as of
    []                    -> test
    ["test"]              -> test
    ["bump-version",vrn]  -> bumpVersion vrn >> gen
    ["test-release",vrn]  -> test_release $ T.pack vrn
    ["commit-message"]    -> commit_message
    ["sdist"]             -> sdist
    ["gen"]               -> gen
    _                     -> do
      let prg = (("  "++pn++" ")++)
      hPutStr stderr $ unlines
        [ "usage:"
        , prg "--help"
        , prg "[test]"
        , prg "bump-version <version>"
        , prg "test-release <version>"
        , prg "commit-message"
        , prg "sdist"
        , prg "gen"
        ]
      exitWith $ ExitFailure 1

test :: IO ()
test = do
  createDirectoryIfMissing False "tmp"
  gen1 "lib/cabal-masters/mega-regex.cabal" "tmp/mega-regex.cabal"
  ok <- cmp "tmp/mega-regex.cabal" "lib/mega-regex.cabal"
  case ok of
    True  -> return ()
    False -> exitWith $ ExitFailure 1

gen :: IO ()
gen = do
  gen1  "lib/cabal-masters/mega-regex.cabal"       "lib/mega-regex.cabal"
  gen1  "lib/cabal-masters/regex.cabal"            "lib/regex.cabal"
  gen1  "lib/cabal-masters/regex-with-pcre.cabal"  "lib/regex-with-pcre.cabal"
  gen1  "lib/cabal-masters/regex-examples.cabal"   "lib/regex-examples.cabal"
  establish "mega-regex" "regex"

gen1 :: FilePath -> FilePath -> IO ()
gen1 in_f out_f = do
    ctx <- setup
    LBS.writeFile out_f =<<
      sed' (gc_script ctx) =<< substVersion_ =<< include =<<
        LBS.readFile in_f

data Ctx =
  Ctx
    { _ctx_w_error             :: IORef Bool
    , _ctx_filter_pcre         :: IORef Bool
    , _ctx_package_constraints :: IORef (Map.Map LBS.ByteString LBS.ByteString)
    , _ctx_test_exe            :: IORef (Maybe TestExe)
    }

data TestExe =
  TestExe
    { _te_test :: Bool
    , _te_exe  :: Bool
    , _te_name :: LBS.ByteString
    , _te_text :: LBS.ByteString
    }
  deriving (Show)

setup :: IO Ctx
setup = Ctx <$> (newIORef True) <*> (newIORef False) <*> (newIORef Map.empty) <*> (newIORef Nothing)

gc_script :: Ctx -> Edits IO RE LBS.ByteString
gc_script ctx = Select
    [ LineEdit [re|^%Werror$|]                            $ w_error_gen              ctx
    , LineEdit [re|^%Wwarn$|]                             $ w_warn_gen               ctx
    , LineEdit [re|^%filter-regex-with-pcre$|]            $ w_filter_pcre            ctx
    , LineEdit [re|^%- +${pkg}(@{%id-}) +${cond}(.*)$|]   $ cond_gen                 ctx
    , LineEdit [re|^%build-depends-${lb}(lib|prog) +${list}(@{%id-}( +@{%id-})*)$|]
                                                          $ build_depends_gen        ctx
    , LineEdit [re|^%test +${i}(@{%id-})$|]               $ test_exe_gen True  False ctx
    , LineEdit [re|^%exe +${i}(@{%id-})$|]                $ test_exe_gen False True  ctx
    , LineEdit [re|^%test-exe +${i}(@{%id-})$|]           $ test_exe_gen True  True  ctx
    , LineEdit [re|^.*$|]                                 $ default_gen              ctx
    ]

w_error_gen, w_warn_gen, w_filter_pcre, cond_gen, build_depends_gen,
  default_gen :: Ctx
              -> LineNo
              -> Matches LBS.ByteString
              -> IO (LineEdit LBS.ByteString)

w_error_gen   Ctx{..} _ _ = writeIORef _ctx_w_error     True  >> return Delete
w_warn_gen    Ctx{..} _ _ = writeIORef _ctx_w_error     False >> return Delete
w_filter_pcre Ctx{..} _ _ = writeIORef _ctx_filter_pcre True  >> return Delete

cond_gen Ctx{..} _ mtchs = do
    modifyIORef _ctx_package_constraints $ Map.insert pkg cond
    return Delete
  where
    pkg  = captureText [cp|pkg|]  mtch
    cond = captureText [cp|cond|] mtch

    mtch = allMatches mtchs !! 0

build_depends_gen ctx@Ctx{..} _ mtchs = do
    we <- readIORef _ctx_w_error
    fp <- readIORef _ctx_filter_pcre
    mp <- readIORef _ctx_package_constraints
    put ctx $ mk_build_depends lb we fp mp lst
  where
    lb   = captureText [cp|lb|] mtch == "lib"
    lst  = LBS.words $ captureText [cp|list|] mtch
    mtch = allMatches mtchs !! 0

default_gen ctx@Ctx{..} _ mtchs = do
    mb <- readIORef _ctx_test_exe
    case mb of
      Nothing -> return $ ReplaceWith ln
      Just te -> case isSpace $ LBS.head $ ln M.<> "\n" of
        True  -> put ctx ln
        False -> adjust_le (M.<>ln) <$> close_test_exe ctx te
  where
    ln   = matchSource mtch
    mtch = allMatches mtchs !! 0

test_exe_gen :: Bool
             -> Bool
             -> Ctx
             -> LineNo
             -> Matches LBS.ByteString
             -> IO (LineEdit LBS.ByteString)
test_exe_gen is_t is_e ctx _ mtchs = do
    mb <- readIORef  (_ctx_test_exe ctx)
    le <- maybe (return Delete) (close_test_exe ctx) mb
    writeIORef (_ctx_test_exe ctx) $ Just $
      TestExe
        { _te_test = is_t
        , _te_exe  = is_e
        , _te_name = i
        , _te_text = ""
        }
    return le
  where
    i    = captureText [cp|i|] mtch

    mtch = allMatches mtchs !! 0

close_test_exe :: Ctx -> TestExe -> IO (LineEdit LBS.ByteString)
close_test_exe ctx@Ctx{..} te = do
  writeIORef _ctx_test_exe Nothing
  put ctx $ mconcat $ concat $
    [ [ mk_test_exe False te "Executable" | _te_exe  te ]
    , [ mk_test_exe True  te "Test-Suite" | _te_test te ]
    ]

put :: Ctx -> LBS.ByteString -> IO (LineEdit LBS.ByteString)
put Ctx{..} lbs = do
    mb <- readIORef _ctx_test_exe
    case mb of
      Nothing -> return $ ReplaceWith lbs
      Just te -> do
        writeIORef _ctx_test_exe $ Just te { _te_text = _te_text te M.<> lbs M.<> "\n" }
        return Delete

mk_test_exe :: Bool -> TestExe -> LBS.ByteString -> LBS.ByteString
mk_test_exe is_t te te_lbs_kw = (M.<> _te_text te) $ LBS.unlines $ concat
    [ [ LBS.pack $ printf "%s %s" (LBS.unpack te_lbs_kw) nm ]
    , [ "    type:               exitcode-stdio-1.0" | is_t ]
    ]
  where
    nm = case is_t of
      True  -> LBS.unpack $ _te_name te M.<> "-test"
      False -> LBS.unpack $ _te_name te

mk_build_depends :: Bool
                 -> Bool
                 -> Bool
                 -> Map.Map LBS.ByteString LBS.ByteString
                 -> [LBS.ByteString]
                 -> LBS.ByteString
mk_build_depends lb we fp mp pks0 = LBS.unlines $
        [ "    Default-Language:   Haskell2010"
        , ""
        ] ++ filter (if lb then const True else const False)
        [ "    Other-Extensions:"
        , "      AllowAmbiguousTypes"
        , "      CPP"
        , "      DeriveDataTypeable"
        , "      DeriveGeneric"
        , "      ExistentialQuantification"
        , "      FlexibleContexts"
        , "      FlexibleInstances"
        , "      FunctionalDependencies"
        , "      GeneralizedNewtypeDeriving"
        , "      MultiParamTypeClasses"
        , "      NoImplicitPrelude"
        , "      OverloadedStrings"
        , "      QuasiQuotes"
        , "      RecordWildCards"
        , "      ScopedTypeVariables"
        , "      TemplateHaskell"
        , "      TypeSynonymInstances"
        , "      UndecidableInstances"
        , ""
        , "    if !impl(ghc >= 8.0)"
        , "      Other-Extensions: TemplateHaskell"
        , "    else"
        , "      Other-Extensions: TemplateHaskellQuotes"
        , ""
        ] ++
        [ "    GHC-Options:"
        , "      -Wall"
        , "      -fwarn-tabs"
        , "      " M.<> w_error_or_warn
        , ""
        , "    Build-depends:"
        ] ++ (map fmt $ zip (True : repeat False) $ L.sortBy comp pks)
  where
    w_error_or_warn = case we of
      True  -> "-Werror"
      False -> "-Wwarn"

    pks = case fp of
      False -> pks0
      True  -> filter (/= "regex-with-pcre") pks0

    fmt (isf,pk) = LBS.pack $
      printf "      %c %-20s %s"
        (if isf then ' ' else ',')
        (LBS.unpack pk)
        (maybe "" LBS.unpack $ Map.lookup pk mp)

    comp x y = case (x=="regex",y=="regex") of
      (True ,True ) -> EQ
      (True ,False) -> LT
      (False,True ) -> GT
      (False,False) -> case (x=="regex-with-pcre",y=="regex-with-pcre") of
        (True ,True ) -> EQ
        (True ,False) -> LT
        (False,True ) -> GT
        (False,False) -> compare x y

adjust_le :: (LBS.ByteString->LBS.ByteString)
          -> LineEdit LBS.ByteString
          -> LineEdit LBS.ByteString
adjust_le f le = case le of
  NoEdit          -> error "adjust_le: not enough context"
  ReplaceWith lbs -> ReplaceWith $ f lbs
  Delete          -> ReplaceWith $ f ""
\end{code}

\begin{code}
sdist :: IO ()
sdist = do
  createDirectoryIfMissing False "tmp"
  sdist'    "regex"            "lib/README-regex.md"
  sdist'    "regex-with-pcre"  "lib/README-regex.md"
  sdist'    "regex-examples"   "lib/README-regex-examples.md"
  establish "mega-regex" "regex"
  vrn <- readCurrentVersion
  let vrn_t = T.pack $ presentVrn vrn
  test_release vrn_t
  smy_t <- summary vrn
  commit_message_ "tmp/commit.txt" vrn smy_t
  SH.shelly $ SH.verbosely $ do
    SH.run_ "git" ["add","--all"]
    SH.run_ "git" ["commit","-F","tmp/commit.txt"]
    SH.run_ "git" ["tag",vrn_t,"-m",smy_t]

sdist' :: T.Text -> SH.FilePath -> IO ()
sdist' nm readme = do
  establish nm nm
  SH.shelly $ SH.verbosely $ do
    SH.cp readme "README.markdown"
    SH.run_ "stack" ["sdist","--stack-yaml","stack-9.2.yaml"]
    (pth,tb) <- analyse_so <$> SH.lastStderr
    SH.cp (SH.fromText $ pth) $ SH.fromText $ "releases/" M.<> tb
  where
    analyse_so so = (mtch!$$[cp|pth|],mtch!$$[cp|tb|])
      where
        mtch = so T.?=~
          [re|^.*Wrote sdist tarball to ${pth}(.*${tb}(regex-.*\.tar\.gz))$|]

establish :: T.Text -> T.Text -> IO ()
establish nm nm' = SH.shelly $ SH.verbosely $ do
    SH.rm_f "mega-regex.cabal"
    SH.rm_f "regex-with-pcre.cabal"
    SH.rm_f "regex.cabal"
    SH.rm_f "regex-examples.cabal"
    SH.cp (SH.fromText sf) (SH.fromText df)
  where
    sf = "lib/" M.<> nm M.<> ".cabal"
    df = nm' M.<> ".cabal"

test_release :: T.Text -> IO ()
test_release vrn_t = do
    setCurrentDirectory "releases"
    SH.shelly $ SH.verbosely $ do
      SH.rm_rf "test-regex-examples"
      unpack "." "regex-examples"
    setCurrentDirectory "test-regex-examples"
    SH.shelly $ SH.verbosely $ do
      unpack ".." "regex"
      unpack ".." "regex-with-pcre"
      SH.cp "../../lib/release-testing/stack.yaml" "."
      SH.run_ "stack" ["--no-terminal","test", "--haddock", "--no-haddock-deps"]
    setCurrentDirectory "../.."
  where
    unpack rp pn = do
        SH.run_ "tar" ["xzf",rp M.<> "/" M.<> pn_vrn M.<> ".tar.gz"]
        SH.mv (SH.fromText pn_vrn) (SH.fromText $ "test-" M.<> pn)
      where
        pn_vrn = pn M.<> "-" M.<> vrn_t
\end{code}


Building the Release Commit Message from the Changelog
------------------------------------------------------

\begin{code}
commit_message :: IO ()
commit_message = do
  createDirectoryIfMissing False "tmp"
  vrn   <- readCurrentVersion
  smy_t <- summary vrn
  commit_message_ "tmp/commit.txt" vrn smy_t
  LBS.readFile "tmp/commit.txt" >>= LBS.putStrLn

commit_message_ :: FilePath -> Vrn -> T.Text -> IO ()
commit_message_ fp vrn@Vrn{} smy_t = do
    rex <- escape ("^"++) vrn_s
    parse_commit smy_ln <$> grepLines rex "changelog" >>= LBS.writeFile fp
  where
    smy_ln = vrn_s ++ ": " ++ T.unpack smy_t
    vrn_s  = presentVrn vrn

parse_commit :: String -> [Line LBS.ByteString] -> LBS.ByteString
parse_commit hdr lns0 = case lns0 of
    _:_:ln:lns | anyMatches $ getLineMatches ln
      -> LBS.unlines $ LBS.pack hdr : map fixes (takeWhile is_bullet lns)
    _ -> error oops
  where
    is_bullet Line{..} =
        LBS.take 4 (matchesSource getLineMatches) == "  * "

    fixes Line{..} =
        matchesSource getLineMatches *=~/ [ed|#${n}([0-9]+)///fixes #${n}|]

    oops = unlines
      [ "failed to parse changelog"
      , "(expected line 3 to start with the current version)"
      ]
\end{code}


Extracting the summary from the Roadmap
---------------------------------------

\begin{code}
summary :: Vrn -> IO T.Text
summary vrn = do
  let vrn_res = concat
        [ show $ _vrn_a vrn
        , "\\."
        , show $ _vrn_b vrn
        , "\\."
        , show $ _vrn_c vrn
        , "\\."
        , show $ _vrn_d vrn
        ]
  rex <- compileRegex $ "- \\[[xX]\\] +@{%date} +v"++vrn_res++" +\\[?${smy}([^]]+)"
  lns <- linesMatched LinesMatched <$> grepLines rex "lib/md/roadmap-incl.md"
  case lns of
    [Line _ (Matches _ [mtch])] -> return $ TE.decodeUtf8 $ LBS.toStrict $ mtch !$$ [cp|smy|]
    _ -> error "failed to locate the summary text in the roadmap"
\end{code}
