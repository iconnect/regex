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
import           Data.Monoid
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
import           Text.RE.TDFA.ByteString.Lazy
import           Text.RE.TDFA.Text                        as T


main :: IO ()
main = do
  (pn,as) <- (,) <$> getProgName <*> getArgs
  case as of
    []                    -> test
    ["test"]              -> test
    ["bump-version",vrn]  -> bumpVersion vrn
    ["sdist"]             -> sdist
    ["gen"]               -> do
      gen  "lib/cabal-masters/mega-regex.cabal"     "lib/mega-regex.cabal"
      gen  "lib/cabal-masters/regex.cabal"          "lib/regex.cabal"
      gen  "lib/cabal-masters/regex-examples.cabal" "lib/regex-examples.cabal"
      establish "mega-regex" "regex"
    _                     -> do
      let prg = (("  "++pn++" ")++)
      hPutStr stderr $ unlines
        [ "usage:"
        , prg "--help"
        , prg "[test]"
        , prg "bump-version <version>"
        , prg "sdist"
        , prg "gen"
        ]
      exitWith $ ExitFailure 1

test :: IO ()
test = do
  createDirectoryIfMissing False "tmp"
  gen "lib/cabal-masters/mega-regex.cabal" "tmp/mega-regex.cabal"
  ok <- cmp "tmp/mega-regex.cabal" "lib/mega-regex.cabal"
  case ok of
    True  -> return ()
    False -> exitWith $ ExitFailure 1

gen :: FilePath -> FilePath -> IO ()
gen in_f out_f = do
    ctx <- setup
    LBS.writeFile out_f =<<
      sed' (gc_script ctx) =<< substVersion_ =<< include =<<
        LBS.readFile in_f

data Ctx =
  Ctx
    { _ctx_w_error             :: IORef Bool
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
setup = Ctx <$> (newIORef True) <*> (newIORef Map.empty) <*> (newIORef Nothing)

gc_script :: Ctx -> SedScript RE
gc_script ctx = Select
    [ (,) [re|^%Werror$|]                                      $ LineEdit $ w_error_gen              ctx
    , (,) [re|^%Wwarn$|]                                       $ LineEdit $ w_warn_gen               ctx
    , (,) [re|^%- +${pkg}(@{%id-}) +${cond}(.*)$|]             $ LineEdit $ cond_gen                 ctx
    , (,) [re|^%build-depends +${list}(@{%id-}( +@{%id-})+)$|] $ LineEdit $ build_depends_gen        ctx
    , (,) [re|^%test +${i}(@{%id-})$|]                         $ LineEdit $ test_exe_gen True  False ctx
    , (,) [re|^%exe +${i}(@{%id-})$|]                          $ LineEdit $ test_exe_gen False True  ctx
    , (,) [re|^%test-exe +${i}(@{%id-})$|]                     $ LineEdit $ test_exe_gen True  True  ctx
    , (,) [re|^.*$|]                                           $ LineEdit $ default_gen              ctx
    ]

w_error_gen, w_warn_gen, cond_gen, build_depends_gen,
  default_gen :: Ctx
              -> LineNo
              -> Matches LBS.ByteString
              -> IO (LineEdit LBS.ByteString)

w_error_gen Ctx{..} _ _ = writeIORef _ctx_w_error True  >> return Delete
w_warn_gen  Ctx{..} _ _ = writeIORef _ctx_w_error False >> return Delete

cond_gen Ctx{..} _ mtchs = do
    modifyIORef _ctx_package_constraints $ Map.insert pkg cond
    return Delete
  where
    pkg  = captureText [cp|pkg|]  mtch
    cond = captureText [cp|cond|] mtch

    mtch = allMatches mtchs !! 0

build_depends_gen ctx@Ctx{..} _ mtchs = do
    we <- readIORef _ctx_w_error
    mp <- readIORef _ctx_package_constraints
    put ctx $ mk_build_depends we mp lst
  where
    lst  = LBS.words $ captureText [cp|list|] mtch
    mtch = allMatches mtchs !! 0

default_gen ctx@Ctx{..} _ mtchs = do
    mb <- readIORef _ctx_test_exe
    case mb of
      Nothing -> return $ ReplaceWith ln
      Just te -> case isSpace $ LBS.head $ ln<>"\n" of
        True  -> put ctx ln
        False -> adjust_le (<>ln) <$> close_test_exe ctx te
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
        writeIORef _ctx_test_exe $ Just te { _te_text = _te_text te <> lbs <> "\n" }
        return Delete

mk_test_exe :: Bool -> TestExe -> LBS.ByteString -> LBS.ByteString
mk_test_exe is_t te te_lbs_kw = (<>_te_text te) $ LBS.unlines $ concat
    [ [ LBS.pack $ printf "%s %s" (LBS.unpack te_lbs_kw) nm ]
    , [ "    type:               exitcode-stdio-1.0" | is_t ]
    ]
  where
    nm = case is_t of
      True  -> LBS.unpack $ _te_name te <> "-test"
      False -> LBS.unpack $ _te_name te

mk_build_depends :: Bool
                 -> Map.Map LBS.ByteString LBS.ByteString
                 -> [LBS.ByteString]
                 -> LBS.ByteString
mk_build_depends we mp pks = LBS.unlines $
        [ "    Default-Language:   Haskell2010"
        , "    GHC-Options:"
        , "      -Wall"
        , "      -fwarn-tabs"
        , "      " <> w_error_or_warn
        , ""
        , "    Build-depends:"
        ] ++ (map fmt $ zip (True : repeat False) $ L.sortBy comp pks)
  where
    w_error_or_warn = case we of
      True  -> "-Werror"
      False -> "-Wwarn"

    fmt (isf,pk) = LBS.pack $
      printf "      %c %-20s %s"
        (if isf then ' ' else ',')
        (LBS.unpack pk)
        (maybe "" LBS.unpack $ Map.lookup pk mp)

    comp x y = case (x=="regex",y=="regex") of
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
  sdist'    "regex"
  sdist'    "regex-examples"
  establish "mega-regex" "regex"
  vrn_t <- T.pack . presentVrn <$> readCurrentVersion
  smy_t <- summary
  SH.shelly $ SH.verbosely $ do
    SH.run_ "git" ["add","--all"]
    SH.run_ "git" ["commit","-m",vrn_t<>": "<>smy_t]
    SH.run_ "git" ["tag",vrn_t,"-m",smy_t]

sdist' :: T.Text -> IO ()
sdist' nm = do
  establish nm nm
  SH.shelly $ SH.verbosely $ do
    SH.cp readme "README.markdown"
    SH.run_ "stack" ["sdist","--stack-yaml","stack-8.0.yaml"]
    (pth,tb) <- analyse_so <$> SH.lastStderr
    SH.cp (SH.fromText $ pth) $ SH.fromText $ "releases/"<>tb
  where
    readme        = SH.fromText $ "lib/README-"<>nm<>".md"

    analyse_so so = (mtch!$$[cp|pth|],mtch!$$[cp|tb|])
      where
        mtch = so T.?=~
          [re|^.*Wrote sdist tarball to ${pth}(.*${tb}(regex-.*\.tar\.gz))$|]

establish :: T.Text -> T.Text -> IO ()
establish nm nm' = SH.shelly $ SH.verbosely $ do
    SH.rm_f "mega-regex.cabal"
    SH.rm_f "regex.cabal"
    SH.rm_f "regex-examples.cabal"
    SH.cp (SH.fromText sf) (SH.fromText df)
  where
    sf = "lib/"<>nm<>".cabal"
    df = nm'<>".cabal"

summary :: IO T.Text
summary = do
  vrn <- SH.liftIO readCurrentVersion
  let vrn_res = concat
        [ show $ _vrn_a vrn
        , "\\."
        , show $ _vrn_b vrn
        , "\\."
        , show $ _vrn_c vrn
        , "\\."
        , show $ _vrn_d vrn
        ]
  rex <- compileRegex () $ "- \\[[xX]\\] +@{%date} +v"++vrn_res++" +\\[?${smy}([^]]+)"
  lns <- linesMatched <$> grepLines rex "lib/md/roadmap-incl.md"
  case lns of
    [Line _ (Matches _ [mtch])] -> return $ TE.decodeUtf8 $ LBS.toStrict $ mtch !$$ [cp|smy|]
    _ -> error "failed to locate the summary text in the roadmap"
\end{code}


let vrn_res = concat [ show $ _vrn_a vrn, "\\.", show $ _vrn_b vrn, "\\.", show $ _vrn_c vrn, "\\.", show $ _vrn_d vrn ]
