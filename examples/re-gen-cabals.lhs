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
import           Prelude.Compat
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           TestKit
import           Text.Printf
import           Text.RE.TDFA.ByteString.Lazy
import           Text.RE.Tools.Sed


main :: IO ()
main = do
  (pn,as) <- (,) <$> getProgName <*> getArgs
  case as of
    []        -> test
    ["test"]  -> test
    ["gen"]   -> gen  "lib/cabal-masters/regex.cabal" "regex.cabal"
    _         -> do
      hPutStrLn stderr $ "usage: " ++ pn ++ " [test|gen]"
      exitWith $ ExitFailure 1

test :: IO ()
test = do
  createDirectoryIfMissing False "tmp"
  gen "lib/cabal-masters/regex.cabal" "tmp/regex.cabal"
  ok <- cmp "tmp/regex.cabal" "regex.cabal"
  case ok of
    True  -> return ()
    False -> exitWith $ ExitFailure 1

gen :: FilePath -> FilePath -> IO ()
gen in_f out_f = do
    ctx <- setup
    substVersion in_f "tmp/regex-vrn.cabal"
    sed (gc_script ctx) "tmp/regex-vrn.cabal" out_f

data Ctx =
  Ctx
    { _ctx_package_constraints :: IORef (Map.Map LBS.ByteString LBS.ByteString)
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
setup = Ctx <$> (newIORef Map.empty) <*> (newIORef Nothing)

gc_script :: Ctx -> SedScript RE
gc_script ctx = Select
    [ (,) [re|^%- +${pkg}(@{%id-}) +${cond}(.*)$|]             $ EDIT_gen $ cond_gen                 ctx
    , (,) [re|^%build-depends +${list}(@{%id-}( +@{%id-})+)$|] $ EDIT_gen $ build_depends_gen        ctx
    , (,) [re|^%test +${i}(@{%id-})$|]                         $ EDIT_gen $ test_exe_gen True  False ctx
    , (,) [re|^%exe +${i}(@{%id-})$|]                          $ EDIT_gen $ test_exe_gen False True  ctx
    , (,) [re|^%test-exe +${i}(@{%id-})$|]                     $ EDIT_gen $ test_exe_gen True  True  ctx
    , (,) [re|^.*$|]                                           $ EDIT_gen $ default_gen              ctx
    ]

cond_gen, build_depends_gen,
  default_gen :: Ctx
              -> LineNo
              -> Matches LBS.ByteString
              -> IO (LineEdit LBS.ByteString)

cond_gen Ctx{..} _ mtchs = do
    modifyIORef _ctx_package_constraints $ Map.insert pkg cond
    return Delete
  where
    pkg  = captureText [cp|pkg|]  mtch
    cond = captureText [cp|cond|] mtch

    mtch = allMatches mtchs !! 0

build_depends_gen ctx@Ctx{..} _ mtchs = do
    mp <- readIORef _ctx_package_constraints
    put ctx $ mk_build_depends mp lst
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

mk_build_depends :: Map.Map LBS.ByteString LBS.ByteString
                 -> [LBS.ByteString]
                 -> LBS.ByteString
mk_build_depends mp pks = LBS.unlines $
        (:) "    Build-depends:" $
          map fmt $ zip (True : repeat False) $
              L.sortBy comp pks
  where
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
