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
import qualified Data.ByteString.Lazy.Char8               as LBS
import           Data.Maybe
import qualified Data.Text                                as T
import           Prelude.Compat
import           System.Environment
import           TestKit
import           Text.RE.Edit
import           Text.RE.TDFA.ByteString.Lazy
\end{code}

\begin{code}
main :: IO ()
main = do
  as  <- getArgs
  case as of
    []                    -> test
    ["test"]              -> test
    [fn,fn'] | is_file fn -> loop fn fn'
    _                     -> usage
  where
    is_file = not . (== "--") . take 2

    usage = do
      prg <- getProgName
      putStr $ unlines
        [ "usage:"
        , "  "++prg++" [test]"
        , "  "++prg++" (-|<in-file>) (-|<out-file>)"
        ]
\end{code}


The Sed Script
--------------

\begin{code}
loop :: FilePath -> FilePath -> IO ()
loop =
  sed $ Select
    [ (,) [re|^%include ${file}(@{%string}) ${rex}(@{%string})$|] $ Function TOP   include_file
    , (,) [re|^.*$|]                                              $ Function TOP $ \_ _ _ _->return Nothing
    ]
\end{code}

\begin{code}
include_file :: LineNo
             -> Match LBS.ByteString
             -> Location
             -> Capture LBS.ByteString
             -> IO (Maybe LBS.ByteString)
include_file _ mtch _ _ = fmap Just $
    extract fp =<< compileRegex () re_s
  where
    fp    = prs_s $ captureText [cp|file|] mtch
    re_s  = prs_s $ captureText [cp|rex|]  mtch

    prs_s = maybe (error "includeDoc") T.unpack . parseString
\end{code}


Extracting a Literate Fragment from a Haskell Program Text
----------------------------------------------------------

\begin{code}
extract :: FilePath -> RE -> IO LBS.ByteString
extract fp rex = extr . LBS.lines <$> LBS.readFile fp
  where
    extr lns =
      case parse $ scan rex lns of
        Nothing      -> oops
        Just (lno,n) -> LBS.unlines $ (hdr :) $ (take n $ drop i lns) ++ [ftr]
          where
            i = getZeroBasedLineNo lno

    oops = error $ concat
      [ "failed to locate fragment matching "
      , show $ reSource rex
      , " in file "
      , show fp
      ]

    hdr  = "<div class='includedcodeblock'>"
    ftr  = "</div>"
\end{code}

\begin{code}
parse :: [Token] -> Maybe (LineNo,Int)
parse []       = Nothing
parse (tk:tks) = case (tk,tks) of
  (Bra b_ln,Hit:Ket k_ln:_) -> Just (b_ln,count_lines_incl b_ln k_ln)
  _                         -> parse tks
\end{code}

\begin{code}
count_lines_incl :: LineNo -> LineNo -> Int
count_lines_incl b_ln k_ln =
  getZeroBasedLineNo k_ln + 1 - getZeroBasedLineNo b_ln
\end{code}

\begin{code}
data Token = Bra LineNo | Hit | Ket LineNo   deriving (Show)
\end{code}

\begin{code}
scan :: RE -> [LBS.ByteString] -> [Token]
scan rex = grepScript
    [ (,) [re|\\begin\{code\}|] $ \i -> chk $ Bra i
    , (,) rex                   $ \_ -> chk   Hit
    , (,) [re|\\end\{code\}|]   $ \i -> chk $ Ket i
    ]
  where
    chk x mtchs = case anyMatches mtchs of
      True  -> Just x
      False -> Nothing
\end{code}


Testing
-------

\begin{code}
test :: IO ()
test = do
  test_pp "include" loop "data/pp-test.lhs" "data/include-result.lhs"
  putStrLn "tests passed"
\end{code}
