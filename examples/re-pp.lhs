\begin{code}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main
  ( main
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8               as LBS
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                                as T
import qualified Shelly                                   as SH
import           System.Directory
import           System.Environment
import           TestKit
import           Text.Printf
import           Text.RE.Edit
import           Text.RE.TDFA.ByteString.Lazy
import           Text.RE.TDFA.Text                as T
import           Text.RE.Tools.Grep
import           Text.RE.Tools.Sed
\end{code}

\begin{code}
main :: IO ()
main = do
  as  <- getArgs
  case as of
    []                          -> test
    ["test"]                    -> test
    ["doc",fn,fn'] | is_file fn -> doc fn fn'
    ["gen",fn,fn'] | is_file fn -> gen fn fn'
    ["all"]                     -> gen_all
    _                           -> usage
  where
    is_file = not . (== "--") . take 2

    doc fn fn' = docMode >>= \dm -> loop dm fn fn'
    gen fn fn' = genMode >>= \gm -> loop gm fn fn'

    usage = do
      pnm <- getProgName
      let prg = ((pnm++" ")++)
      putStr $ unlines
        [ "usage:"
        , "  "++prg "--help"
        , "  "++prg "[test]"
        , "  "++prg "all"
        , "  "++prg "doc (-|<in-file>) (-|<out-file>)"
        , "  "++prg "gen (-|<in-file>) (-|<out-file>)"
        ]
\end{code}


The Sed Script
--------------

\begin{code}
-- | the MODE determines whether we are generating documentation
-- or a Haskell testsuite and includes any IO-accessible state
-- needed by the relevant processor
data MODE
  = Doc DocState  -- ^ document-generation state
  | Gen GenState  -- ^ adjusting-the-program-for-testing state
\end{code}

The `DocState` is initialised to `Outside` and flips though the different
states as it traverses a code block, so that we can wrap code
blocks in special <div class="replcodeblock"> blocks when their
first line indicates that it contains a REPL calculation, which the
style sheet can pick up and present accordingly.

\begin{code}
data DocMode
  = Outside     -- not inside a begin{code} ... \end{code} block
  | Beginning   -- at the start of a begin{code} ... \end{code} block
  | InsideRepl  -- inside a REPL code block
  | InsideProg  -- inside a non-REPL code block
  deriving (Eq,Show)

type DocState = IORef DocMode

genMode :: IO MODE
genMode = Gen <$> newIORef []
\end{code}

\begin{code}
-- | the state is the accumulated test function identifiers for
-- generating the list of them gets added to the end of the programme
type GenState = IORef [String]

docMode :: IO MODE
docMode = Doc <$> newIORef Outside
\end{code}


\begin{code}
loop :: MODE -> FilePath -> FilePath -> IO ()
loop mode =
  sed $ Select
    [ (,) [re|^%include ${file}(@{%string}) ${rex}(@{%string})$|]      $ EDIT_fun TOP $ include mode
    , (,) [re|^%main ${arg}(top|bottom)$|]                             $ EDIT_gen     $ main_   mode
    , (,) [re|^\\begin\{code\}$|]                                      $ EDIT_gen     $ begin   mode
    , (,) [re|^${fn}(evalme@{%id}) = checkThis ${arg}(@{%string}).*$|] $ EDIT_fun TOP $ evalme  mode
    , (,) [re|^\\end\{code\}$|]                                        $ EDIT_fun TOP $ end     mode
    , (,) [re|^.*$|]                                                   $ EDIT_fun TOP $ other   mode
    ]
\end{code}

\begin{code}
include, evalme, end,
  other :: MODE
        -> LineNo
        -> Match LBS.ByteString
        -> Location
        -> Capture LBS.ByteString
        -> IO (Maybe LBS.ByteString)

main_,
  begin :: MODE
        -> LineNo
        -> Matches LBS.ByteString
        -> IO (LineEdit LBS.ByteString)

include (Doc _ ) = includeDoc
include (Gen _ ) = passthru

main_   (Doc _ ) = mainDoc
main_   (Gen gs) = mainGen    gs

begin   (Doc ds) = beginDoc   ds
begin   (Gen _ ) = passthru_g

evalme  (Doc ds) = evalmeDoc  ds
evalme  (Gen gs) = evalmeGen  gs

end     (Doc ds) = endDoc     ds
end     (Gen _ ) = passthru

other   (Doc ds) = otherDoc   ds
other   (Gen _ ) = passthru

passthru :: LineNo
         -> Match LBS.ByteString
         -> Location
         -> Capture LBS.ByteString
         -> IO (Maybe LBS.ByteString)
passthru _ _ _ _ = return Nothing

passthru_g :: LineNo
           -> Matches LBS.ByteString
           -> IO (LineEdit LBS.ByteString)
passthru_g _ _ = return NoEdit
\end{code}


Script to Generate All Tutorial Tests and Docs
----------------------------------------------

\begin{code}
gen_all :: IO ()
gen_all = do
    -- prepare HTML docs for the (literate) tools
    pd "re-gen-modules"
    pd "re-include"
    pd "re-nginx-log-processor"
    pd "re-pp"
    pd "re-tests"
    pd "TestKit"
    pd "RE/Capture"
    pd "RE/Edit"
    pd "RE/IsRegex"
    pd "RE/Options"
    pd "RE/Replace"
    pd "RE/TestBench"
    pd "RE/Tools/Grep"
    pd "RE/Tools/Lex"
    pd "RE/Tools/Sed"
    pd "RE/Internal/NamedCaptures"
    -- render the tutorial in HTML
    dm <- docMode
    loop dm "examples/re-tutorial-master.lhs" "tmp/re-tutorial.lhs"
    createDirectoryIfMissing False "tmp"
    pandoc'
      "re-tutorial.lhs"
      "examples/re-tutorial.lhs"
      "tmp/re-tutorial.lhs"
      "docs/re-tutorial.html"
    -- generate the tutorial-based tests
    gm <- genMode
    loop gm "examples/re-tutorial-master.lhs" "examples/re-tutorial.lhs"
    putStrLn ">> examples/re-tutorial.lhs"
  where
    pd fnm = case captureTextMaybe [cp|mnm|] $ fnm T.?=~ [re|^RE/(Tools/|Internal/)?${mnm}(@{%id})|] of
        Just mnm -> pandoc fnm ("Text/"<>fnm<>".lhs") ("docs/"<>mnm<>".html")
        Nothing  -> pandoc fnm (  "examples/"<>fnm<>".lhs") ("docs/"<>fnm<>".html")
\end{code}


Generating the Tutorial
-----------------------

\begin{code}
includeDoc :: LineNo
           -> Match LBS.ByteString
           -> Location
           -> Capture LBS.ByteString
           -> IO (Maybe LBS.ByteString)
includeDoc _ mtch _ _ = fmap Just $
    extract fp =<< compileRegex () re_s
  where
    fp    = prs_s $ captureText [cp|file|] mtch
    re_s  = prs_s $ captureText [cp|rex|]  mtch

    prs_s = fromMaybe (error "includeDoc") . parseString
\end{code}

\begin{code}
mainDoc :: LineNo
        -> Matches LBS.ByteString
        -> IO (LineEdit LBS.ByteString)
mainDoc _ _ = return Delete
\end{code}

\begin{code}
beginDoc :: DocState
         -> LineNo
         -> Matches LBS.ByteString
         -> IO (LineEdit LBS.ByteString)
beginDoc ds _ _ = writeIORef ds Beginning >> return Delete
\end{code}

\begin{code}
evalmeDoc, endDoc, otherDoc :: DocState
                            -> LineNo
                            -> Match LBS.ByteString
                            -> Location
                            -> Capture LBS.ByteString
                            -> IO (Maybe LBS.ByteString)

evalmeDoc ds lno _ _ _ = do
  dm <- readIORef ds
  when (dm/=Beginning) $
    bad_state "evalme" lno dm
  writeIORef ds InsideRepl
  return $ Just $ "<div class=\"replcodeblock\">\n"<>begin_code

endDoc    ds lno _ _ _ = do
  dm <- readIORef ds
  case dm of
    Outside    -> bad_state "end" lno dm
    Beginning  -> return $ Just $ begin_code <> "\n" <> end_code
    InsideRepl -> return $ Just $ end_code   <> "\n</div>"
    InsideProg -> return   Nothing

otherDoc  ds _ mtch _ _ = do
  dm <- readIORef ds
  case dm of
    Beginning -> do
      writeIORef ds InsideProg
      return $ Just $ begin_code <> "\n" <> matchSource mtch
    _ -> return Nothing

bad_state :: String -> LineNo -> DocMode -> IO a
bad_state lab lno dm = error $
  printf "Bad document syntax: %s: %d: %s" lab (getLineNo lno) $ show dm
\end{code}


Generating the Tests
--------------------

\begin{code}
evalmeGen :: GenState
          -> LineNo
          -> Match LBS.ByteString
          -> Location
          -> Capture LBS.ByteString
          -> IO (Maybe LBS.ByteString)
evalmeGen gs _ mtch0 _ _ = Just <$>
    replaceCapturesM replace_ ALL f mtch0
  where
    f mtch loc cap = case _loc_capture loc of
      2 -> do
          modifyIORef gs (ide:)
          return $ Just $ LBS.pack $ show ide
        where
          ide = LBS.unpack $ captureText [cp|fn|] mtch
      _ -> return $ Just $ capturedText cap
\end{code}

How are we doing?

\begin{code}
mainGen :: GenState
        -> LineNo
        -> Matches LBS.ByteString
        -> IO (LineEdit LBS.ByteString)
mainGen gs _ mtchs = case allMatches mtchs of
  [mtch]  ->
    case captureText [cp|arg|] $ mtch of
      "top"    -> return $ ReplaceWith $ LBS.unlines $
          [ begin_code
          , "module Main(main) where"
          , end_code
          , ""
          , "*********************************************************"
          , "*"
          , "* WARNING: this is generated from pp-tutorial-master.lhs "
          , "*"
          , "*********************************************************"
          ]
      "bottom" -> do
        fns <- readIORef gs
        return $ ReplaceWith $ LBS.unlines $
          [ begin_code
          , "main :: IO ()"
          , "main = runTests"
          ] ++ mk_list fns ++
          [ end_code
          ]
      _ -> error "mainGen (b)"
  _ -> error "mainGen (a)"
\end{code}

We cannot place these strings inline without confusing pandoc so we
use these definitions instead.

\begin{code}
begin_code, end_code :: LBS.ByteString
begin_code = "\\"<>"begin{code}"
end_code   = "\\"<>"end{code}"
\end{code}



\begin{code}
mk_list :: [String] -> [LBS.ByteString]
mk_list []          = ["[]"]
mk_list (ide0:ides) = f "[" ide0 $ foldr (f ",") ["  ]"] ides
  where
    f pfx ide t = ("  "<>pfx<>" "<>LBS.pack ide) : t
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


pandoc
------

\begin{code}
pandoc :: T.Text -> T.Text -> T.Text -> IO ()
pandoc title in_file = pandoc' title in_file in_file

pandoc' :: T.Text -> T.Text -> T.Text -> T.Text -> IO ()
pandoc' title repo_path in_file out_file = do
  writeFile "tmp/bc.html" bc
  fmap (const ()) $
    SH.shelly $ SH.verbosely $
      SH.run "pandoc"
        [ "-f", "markdown+lhs+grid_tables"
        , "-t", "html"
        , "-s"
        , "-B", "tmp/bc.html"
        , "-c", "lib/styles.css"
        , "-c", "lib/bs.css"
        , "-o", out_file
        , in_file
        ]
  where
    bc = concat
      [ "<ol class='breadcrumb'><li><a href='.' title='Home'>"
      , "Home</a></li> &gt; <a title='source file' href='"
      , repo_url
      , "'>"
      , T.unpack title
      , "</a></ol>"
      ]

    repo_url = concat
      [ "https://github.com/iconnect/regex/blob/master/"
      , T.unpack repo_path
      ]
\end{code}

testing
-------

\begin{code}
test :: IO ()
test = do
  dm <- docMode
  test_pp "pp-doc" (loop dm) "data/pp-test.lhs" "data/pp-result-doc.lhs"
  gm <- genMode
  test_pp "pp-gen" (loop gm) "data/pp-test.lhs" "data/pp-result-gen.lhs"
  putStrLn "tests passed"
\end{code}
