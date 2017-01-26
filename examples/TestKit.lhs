\begin{code}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}

module TestKit
  ( Test
  , runTests
  , checkThis
  , test_pp
  ) where

import           Control.Exception
import qualified Data.Text                                as T
import qualified Shelly                                   as SH
import           System.Environment
import           System.Exit
import           System.IO
\end{code}

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
