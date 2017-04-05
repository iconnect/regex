Example: Sort-Import Processor
==============================

This example looks for Haskell files and sorts their import statements
into a standard (alphabetical) order

\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main
  ( main
  ) where

import           Control.Applicative
import qualified Control.Monad                            as M
import qualified Data.ByteString.Lazy.Char8               as LBS
import           Prelude.Compat
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           TestKit
import           Text.Printf
import           Text.RE.TDFA.String
import           Text.RE.Tools.Find
\end{code}

Mode
----

This program can run in one of two modes.

\begin{code}
data Mode
    = Check           -- only check for unsorted files, generating an
                      -- error if any not sorted
    | Update          -- update any unsorted files
  deriving (Eq,Show)
\end{code}

\begin{code}
main :: IO ()
main = do
  as  <- getArgs
  case as of
    []                         -> test
    ["test"]                   -> test
    ["update",fp] | is_file fp -> sort_r Update fp
    ["check" ,fp] | is_file fp -> sort_r Check  fp
    _                          -> usage
  where
    is_file = not . (== "--") . take 2

    test = do
      sort_r Check "Text"
      sort_r Check "examples"

    usage = do
      prg <- getProgName
      putStr $ unlines
        [ "usage:"
        , "  "++prg++" [test]"
        , "  "++prg++" <directory>"
        ]
\end{code}


The Find Script
---------------

\begin{code}
sort_r :: Mode -> FilePath -> IO ()
sort_r md root = findMatches_ fm [re|\.l?hs|] root >>= sort_these md root
  where
    fm = FindMethods
      { doesDirectoryExistDM = doesDirectoryExist
      , listDirectoryDM      = getDirectoryContents
      , combineDM            = (</>)
      }
\end{code}


Processing the List of Files
----------------------------

\begin{code}
sort_these :: Mode -> FilePath -> [FilePath] -> IO ()
sort_these md root fps = do
  ok <- and <$> mapM (sort_this md) fps
  case ok of
    True  -> msg "all imports sorted"
    False -> case md of
      Check  -> do
        msg "Some imports need sorting"
        exitWith $ ExitFailure 1
      Update ->
        msg "Some imports were sorted"
  where
    msg :: String -> IO ()
    msg s = printf "%-10s : %s\n" root s
\end{code}


Processing a single File
------------------------

\begin{code}
sort_this :: Mode -> FilePath -> IO Bool
sort_this md fp = LBS.readFile fp >>= sort_this'
  where
    sort_this' lbs = do
        M.when (not same)   $ putStrLn fp
        M.when (md==Update) $ LBS.writeFile fp lbs'
        return same
      where
        same = lbs==lbs'
        lbs' = sortImports lbs
\end{code}


Sorting the Imports of the Text of a Haskell Script
---------------------------------------------------

The function for sorting a Haskell script, `sortImports` has been
placed in `TestKit` so that it can be shared with re-gen-modules`.

%include "examples/TestKit.lhs" "sortImports ::"
