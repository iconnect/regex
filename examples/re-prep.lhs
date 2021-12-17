Regex (Page) Prep
=================

This tool turns the markdown and literate Haskell into the HTML that
makes up the website and the README.md for GitHub and for Hackage
(based on the index.md for the website) and the test suite based on
the tutorial.

The tool is self-testing: run it with no arguments (or `cabal test`).


\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main
  ( main
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8                    as B
import qualified Data.ByteString.Lazy.Char8               as LBS
import           Data.IORef
import qualified Data.List                                as L
import           Data.Maybe
import qualified Data.Monoid                              as M
import qualified Data.Text                                as T
-- import           Network.HTTP.Conduit
import           Prelude.Compat
import qualified Shelly                                   as SH
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import           TestKit
import           Text.Heredoc
import           Text.RE.Replace
import           Text.RE.TDFA.ByteString.Lazy
import qualified Text.RE.TDFA.String                      as TS
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
    ["badges"]                  -> badges
    ["blog-badge"]              -> blog_badge
    ["pages"]                   -> pages
    ["all"]                     -> gen_all
    _                           -> usage
  where
    is_file = not . (== "--") . take 2

    doc fn fn' =                    prepare_tutorial Doc fn fn'
    gen fn fn' = genMode >>= \gm -> prepare_tutorial gm  fn fn'

    usage = do
      pnm <- getProgName
      let prg = (("  "++pnm++" ")++)
      hPutStr stderr $ unlines
        [ "usage:"
        , prg "--help"
        , prg "[test]"
        , prg "badges"
        , prg "blog-badge"
        , prg "pages"
        , prg "all"
        , prg "doc (-|<in-file>) (-|<out-file>)"
        , prg "gen (-|<in-file>) (-|<out-file>)"
        ]
\end{code}


Script to Generate the Whole Web Site
-------------------------------------

\begin{code}
gen_all :: IO ()
gen_all = do
    -- prepare HTML docs for the (literate) tools
    pd "re-gen-cabals"
    pd "re-gen-modules"
    pd "re-include"
    pd "re-nginx-log-processor"
    pd "re-prep"
    pd "re-sort-imports"
    pd "re-top"
    pd "re-tests"
    pd "TestKit"
    pd "RE/REOptions"
    pd "RE/Tools/Edit"
    pd "RE/Tools/Grep"
    pd "RE/Tools/Sed"
    pd "RE/ZeInternals/NamedCaptures"
    pd "RE/ZeInternals/Replace"
    pd "RE/ZeInternals/TestBench"
    pd "RE/ZeInternals/Tools/Lex"
    pd "RE/ZeInternals/Types/IsRegex"
    pd "RE/ZeInternals/Types/Matches"
    pd "RE/ZeInternals/Types/Match"
    pd "RE/ZeInternals/Types/Capture"
    -- render the tutorial in HTML
    createDirectoryIfMissing False "tmp"
    gen_tutorial "tutorial"
    gen_tutorial "tutorial-options"
    gen_tutorial "tutorial-replacing"
    gen_tutorial "tutorial-testbench"
    gen_tutorial "tutorial-tools"
    pages
  where
    pd fnm = case (mtch !$$? [cp|fdr|],mtch !$$? [cp|mnm|]) of
        (Nothing ,Just mnm) -> pandoc_lhs ("Text.RE."          ++mnm) ("Text/"    ++fnm++".lhs") ("docs/"++mnm++".html")
        (Just fdr,Just mnm) -> pandoc_lhs ("Text.RE."++fdr++"."++mnm) ("Text/"    ++fnm++".lhs") ("docs/"++mnm++".html")
        _                   -> pandoc_lhs ("examples/"++fnm++".lhs" ) ("examples/"++fnm++".lhs") ("docs/"++fnm++".html")
      where
        mtch = fnm TS.?=~ [re|^RE/(${fdr}([a-zA-Z/]+)/)?${mnm}(@{%id})|]
\end{code}


\begin{code}
gen_tutorial :: String -> IO ()
gen_tutorial nm = do
    prepare_tutorial Doc ("examples" </> mst) ("tmp" </> tgt)
    pandoc_lhs'       tgt
      ("examples" </> tgt)
      ("tmp"      </> tgt)
      ("docs"     </> htm)
    -- generate the tutorial-based tests
    gm <- genMode
    prepare_tutorial gm ("examples" </> mst) ("examples" </> tgt)
    putStrLn $ ">> " ++ ("examples" </> tgt)
  where
    tgt = "re-" ++ nm ++ ".lhs"
    htm = "re-" ++ nm ++ ".html"
    mst = "re-" ++ nm ++ "-master.lhs"
\end{code}


The Tutorial Preprocessor
-------------------------

\begin{code}
-- | the MODE determines whether we are generating documentation
-- or a Haskell testsuite and includes any IO-accessible state
-- needed by the relevant processor
data MODE
  = Doc           -- ^ generating mardown+lhs input for pandoc
  | Gen GenState  -- ^ adjusting-the-program-for-testing state
\end{code}

The `DocState` is initialised to `Outside` and flips though the different
states as it traverses a code block, so that we can wrap code
blocks in special `<div class="replcodeblock">` blocks when their
first line indicates that it contains a REPL calculation, which the
style sheet can pick up and present accordingly.

\begin{code}
-- | the state is the accumulated test function identifiers for
-- generating the list of them gets added to the end of the programme
type GenState = IORef [String]

genMode :: IO MODE
genMode = Gen <$> newIORef []
\end{code}

\begin{code}
prepare_tutorial :: MODE -> FilePath -> FilePath -> IO ()
prepare_tutorial mode fp_in fp_out =
  LBS.readFile fp_in >>= prep_tutorial_pp mode >>= incld >>=
    LBS.writeFile fp_out . sortImports
  where
    incld = case mode of
      Doc   -> include_code_pp
      Gen _ -> return
\end{code}

\begin{code}
prep_tutorial_pp :: MODE -> LBS.ByteString -> IO LBS.ByteString
prep_tutorial_pp mode =
  sed' $ Select
    [ LineEdit [re|^%main ${arg}(top|bottom)$|]                            $ main_    mode
    , LineEdit [re|^import *TestKit$|]                                     $ hide     mode
    , LineEdit [re|^\{-# OPTIONS_GHC -fno-warn-missing-signatures *#-\}$|] $ hide     mode
    , Function [re|^${fn}(evalme@{%id}) += +(checkThis|checkThisWith +@{%id}) +${arg}(@{%string}) +\(${ans}([^)]+)\) +\$ +\(${exp}(.*)\)$|]
                                                                       TOP $ evalme   mode
    , Function [re|^${fn}(evalme@{%id}) += +(checkThis|checkThisWith +@{%id}) +${arg}(@{%string}) +\(${ans}([^)]+)\) +\$ +${exp}(.*)$|]
                                                                       TOP $ evalme   mode
    , Function [re|^.*$|]                                              TOP $ passthru
    ]
\end{code}

\begin{code}
evalme :: MODE
       -> LineNo
       -> Match LBS.ByteString
       -> RELocation
       -> Capture LBS.ByteString
       -> IO (Maybe LBS.ByteString)
evalme  Doc     = evalmeDoc
evalme (Gen gs) = evalmeGen  gs

main_ :: MODE
      -> LineNo
      -> Matches LBS.ByteString
      -> IO (LineEdit LBS.ByteString)
main_   Doc     = delete
main_  (Gen gs) = mainGen    gs

hide :: MODE
     -> LineNo
     -> Matches LBS.ByteString
    -> IO (LineEdit LBS.ByteString)
hide  Doc    = delete
hide (Gen _) = passthru_
\end{code}

\begin{code}
evalmeDoc :: LineNo
          -> Match LBS.ByteString
          -> RELocation
          -> Capture LBS.ByteString
          -> IO (Maybe LBS.ByteString)
evalmeDoc _ mtch _ _ = return $ Just $ flip replace mtch $ LBS.intercalate "\n"
  [ "ghci> ${exp}"
  , "${ans}"
  ]
\end{code}

\begin{code}
evalmeGen :: GenState
          -> LineNo
          -> Match LBS.ByteString
          -> RELocation
          -> Capture LBS.ByteString
          -> IO (Maybe LBS.ByteString)
evalmeGen gs _ mtch0 _ _ = Just <$>
    replaceCapturesM replaceMethods ALL f mtch0
  where
    f mtch loc _ =
      case locationCapture loc == arg_i of
        True  -> do
            modifyIORef gs (ide:)
            return $ Just $ LBS.pack $ show ide
          where
            ide = LBS.unpack $ captureText [cp|fn|] mtch
        False -> return Nothing

    arg_i = either oops id $ findCaptureID [cp|arg|] $ captureNames mtch0

    oops  = error "evalmeGen: confused captures!"
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
          , "main = runTheTests"
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
begin_code = "\\" M.<>  "begin{code}"
end_code   = "\\" M.<>  "end{code}"
\end{code}

\begin{code}
mk_list :: [String] -> [LBS.ByteString]
mk_list []          = ["  []"]
mk_list (ide0:ides) = f "[" ide0 $ foldr (f ",") ["  ]"] ides
  where
    f pfx ide t = ("  " M.<> pfx M.<> " " M.<> LBS.pack ide) : t
\end{code}


include_code_pp
---------------

\begin{code}
include_code_pp :: LBS.ByteString -> IO LBS.ByteString
include_code_pp =
  sed' $ Select
    [ Function [re|^%include ${file}(@{%string}) ${rex}(@{%string})$|] TOP  inc_code
    , Function [re|^.*$|]                                              TOP  passthru
    ]
\end{code}

\begin{code}
inc_code :: LineNo
         -> Match LBS.ByteString
         -> RELocation
         -> Capture LBS.ByteString
         -> IO (Maybe LBS.ByteString)
inc_code _ mtch _ _ = fmap Just $
    extract fp =<< compileRegex re_s
  where
    fp    = prs_s $ captureText [cp|file|] mtch
    re_s  = prs_s $ captureText [cp|rex|]  mtch

    prs_s = maybe (error "include_code") T.unpack . parseString
\end{code}


passthru and delete actions
---------------------------

\begin{code}
passthru :: LineNo
         -> Match LBS.ByteString
         -> RELocation
         -> Capture LBS.ByteString
         -> IO (Maybe LBS.ByteString)
passthru _ _ _ _ = return Nothing

passthru_ :: LineNo
          -> Matches LBS.ByteString
          -> IO (LineEdit LBS.ByteString)
passthru_ _ _ = return NoEdit

delete :: LineNo
       -> Matches LBS.ByteString
       -> IO (LineEdit LBS.ByteString)
delete _ _ = return Delete
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
scan rex = grepWithScript
    [ (,) [re|\\begin\{code\}|] $ \i -> chk $ Bra i
    , (,) rex                   $ \_ -> chk   Hit
    , (,) [re|\\end\{code\}|]   $ \i -> chk $ Ket i
    ]
  where
    chk x mtchs = case anyMatches mtchs of
      True  -> Just x
      False -> Nothing
\end{code}


badges
------

\begin{code}
badges :: IO ()
badges = do
    mapM_ collect
      [ (,) "license"             "https://img.shields.io/badge/license-BSD3-brightgreen.svg"
      , (,) "unix-build"          "https://img.shields.io/travis/iconnect/regex.svg?label=Linux%2BmacOS"
      , (,) "windows-build"       "https://img.shields.io/appveyor/ci/cdornan/regex.svg?label=Windows"
      , (,) "coverage"            "https://img.shields.io/coveralls/iconnect/regex.svg"
      , (,) "build-status"        "https://img.shields.io/travis/iconnect/regex.svg?label=Build%20Status"
      , (,) "maintainers-contact" "https://img.shields.io/badge/email-maintainers%40regex.uk-blue.svg"
      , (,) "feedback-contact"    "https://img.shields.io/badge/email-feedback%40regex.uk-blue.svg"
      ]
  where
    collect (nm,url) = do
      putStrLn $ "updating badge: " ++ nm
      simpleHttp url >>= LBS.writeFile (badge_fn nm)

    badge_fn nm = "docs/badges/"++nm++".svg"

    simpleHttp :: MonadIO m => String -> m LBS.ByteString
    simpleHttp = undefined -- TODO: fixme
\end{code}


blog_badge
----------

\begin{code}
blog_badge :: IO ()
blog_badge = do
  dts <- L.sortBy (flip compare) . map (take 10) <$>
            getDirectoryContents "../regex-blog/posts"
  case dts of
    []   -> error "No posts found!"
    dt:_ -> case matched $ dt_lbs ?=~ date_re of
        False -> error $ "Post date format not recognised: " ++ dt
        True  -> do
          putStrLn $ "Latest blog is: " ++ dt
          lbs <- lbsReadFile badges_file
          LBS.writeFile badges_file $ replaceAll dt_lbs $ lbs *=~ date_re
      where
        dt_lbs      = LBS.pack dt
  where
    date_re     = [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
    badges_file = "docs/badges/blog.svg"
\end{code}


pages
-----

\begin{code}
pages :: IO ()
pages = do
  prep_page "regex"          MM_hackage "lib/md/index.md" "lib/README-regex.md"
  prep_page "regex-examples" MM_hackage "lib/md/index.md" "lib/README-regex-examples.md"
  prep_page "regex"          MM_github  "lib/md/index.md" "README.md"
  mapM_ pandoc_page [minBound..maxBound]
\end{code}

\begin{code}
data Page
  = PG_index
  | PG_about
  | PG_reblog
  | PG_contact
  | PG_build_status
  | PG_installation
  | PG_tutorial
  | PG_examples
  | PG_roadmap
  | PG_macros
  | PG_directory
  | PG_changelog
  deriving (Bounded,Enum,Eq,Ord,Show)

page_root :: Page -> String
page_root = map tr . drop 3 . show
  where
    tr '_' = '-'
    tr c   = c

page_master_file, page_docs_file :: Page -> FilePath
page_master_file pg = "lib/md/" ++ page_root pg ++ ".md"
page_docs_file   pg = "docs/"   ++ page_root pg ++ ".html"

page_address :: Page -> LBS.ByteString
page_address PG_reblog = "blog"
page_address pg        = LBS.pack $ page_root pg

page_title :: Page -> LBS.ByteString
page_title pg = case pg of
  PG_index        -> "Home"
  PG_about        -> "About"
  PG_reblog       -> "Blog"
  PG_contact      -> "Contact"
  PG_build_status -> "Build Status"
  PG_installation -> "Installation"
  PG_tutorial     -> "Tutorial"
  PG_examples     -> "Examples"
  PG_roadmap      -> "Roadmap"
  PG_macros       -> "Macro Tables"
  PG_directory    -> "Directory"
  PG_changelog    -> "Change Log"
\end{code}

\begin{code}
pandoc_page :: Page -> IO ()
pandoc_page pg = do
  mt_lbs <- setup_ttl <$> LBS.readFile (page_master_file pg)
  (hdgs,md_lbs) <- prep_page' MM_pandoc mt_lbs
  LBS.writeFile "tmp/metadata.markdown"  $ LBS.unlines ["---","title: " M.<> page_title pg,"---"]
  LBS.writeFile "tmp/heading.markdown"   $ page_heading pg
  LBS.writeFile "tmp/page_pre_body.html" $ mk_pre_body_html pg hdgs
  LBS.writeFile "tmp/page_pst_body.html"   pst_body_html
  LBS.writeFile "tmp/page.markdown"        md_lbs
  SH.shelly $ SH.verbosely $
    SH.run_ "pandoc"
      [ "-f", "markdown+grid_tables+autolink_bare_uris"
      , "-t", "html5"
      , "-T", "regex"
      , "-s"
      , "-H", "lib/favicons.html"
      , "-B", "tmp/page_pre_body.html"
      , "-A", "tmp/page_pst_body.html"
      , "-c", "lib/styles.css"
      , "-o", T.pack $ page_docs_file pg
      , "tmp/metadata.markdown"
      , "tmp/heading.markdown"
      , "tmp/page.markdown"
      ]
  where
    setup_ttl = case pg of
      PG_index -> set_title "regex"
      _        -> id

data Heading =
  Heading
    { _hdg_id    :: LBS.ByteString
    , _hdg_title :: LBS.ByteString
    }
  deriving (Show)

data MarkdownMode
  = MM_github
  | MM_hackage
  | MM_pandoc
  deriving (Eq,Show)

page_heading :: Page -> LBS.ByteString
page_heading PG_index = ""
page_heading pg       =
  "<p class='pagebc'><a href='.' title='Home'>Home</a> &raquo; **" M.<> page_title pg M.<> "**</p>\n"

prep_page :: LBS.ByteString -> MarkdownMode -> FilePath -> FilePath -> IO ()
prep_page ttl mmd in_fp out_fp = do
  lbs      <- set_title ttl <$> LBS.readFile in_fp
  (_,lbs') <- prep_page' mmd lbs
  LBS.writeFile out_fp lbs'

set_title :: LBS.ByteString -> LBS.ByteString -> LBS.ByteString
set_title ttl lbs = fromMaybe oops $ flip sed' lbs $ Pipe
    [ Function [re|<<\$title\$>>|] TOP $ \_ _ _ _->return $ Just ttl
    ]
  where
    -- runIdentity added to base in 4.9 only
    oops = error "set_title"

prep_page' :: MarkdownMode -> LBS.ByteString -> IO ([Heading],LBS.ByteString)
prep_page' mmd lbs = do
    rf_h <- newIORef []
    rf_t <- newIORef False
    lbs1 <- fmap (tweak_md mmd) $ sed' (scr rf_h rf_t) =<< include lbs
    lbs2 <- fromMaybe "" <$> fin_task_list' mmd rf_t
    hdgs <- reverse <$> readIORef rf_h
    return (hdgs,lbs1 M.<> lbs2)
  where
    scr rf_h rf_t = Select
      [ Function [re|^%heading#${ide}(@{%id}) +${ttl}([^ ].*)$|] TOP $ heading       mmd rf_t rf_h
      , Function [re|^- \[ \] +${itm}(.*)$|]                     TOP $ task_list     mmd rf_t False
      , Function [re|^- \[[Xx]\] +${itm}(.*)$|]                  TOP $ task_list     mmd rf_t True
      , Function [re|^.*$|]                                      TOP $ fin_task_list mmd rf_t
      ]

heading :: MarkdownMode
        -> IORef Bool
        -> IORef [Heading]
        -> LineNo
        -> Match LBS.ByteString
        -> RELocation
        -> Capture LBS.ByteString
        -> IO (Maybe LBS.ByteString)
heading mmd rf_t rf_h _ mtch _ _ = do
    lbs <- fromMaybe "" <$> fin_task_list' mmd rf_t
    modifyIORef rf_h (Heading ide ttl:)
    return $ Just $ lbs M.<> h2
  where
    h2 = case mmd of
      MM_github  -> "## " M.<> ttl
      MM_hackage -> "## " M.<> ttl
      MM_pandoc  -> "<h2 id='" M.<> ide M.<> "'>" M.<> ttl M.<> "</h2>"

    ide = mtch !$$ [cp|ide|]
    ttl = mtch !$$ [cp|ttl|]

mk_pre_body_html :: Page -> [Heading] -> LBS.ByteString
mk_pre_body_html pg hdgs = hdr M.<> LBS.concat (map nav [minBound..maxBound]) M.<> ftr
  where
    hdr :: LBS.ByteString
    hdr = [here|    <div id="container">
    <div id="sidebar">
      <div id="corner">
        |] M.<> branding M.<> [here|
      </div>
      <div class="widget" id="pages">
        <ul class="page-nav">
|]

    nav dst_pg = LBS.unlines $
      nav_li "          " pg_cls pg_adr pg_ttl :
        [ nav_li "            " ["secnav"] ("#" M.<> _hdg_id) _hdg_title
          | Heading{..} <- hdgs
          , is_moi
          ]
      where
        pg_cls = ["pagenav",if is_moi then "moi" else "toi"]
        pg_adr = page_address dst_pg
        pg_ttl = page_title   dst_pg
        is_moi = pg == dst_pg

    nav_li pfx cls dst title = LBS.concat
      [ pfx
      , "<li class='"
      , LBS.unwords cls
      , "'><a href='"
      , dst
      , "'>"
      , title
      , "</a></li>"
      ]

    ftr = [here|          </ul>
      </div>
      <div class="supplementary widget" id="github">
        <a href="http://code.regex.uk"><img src="images/code.svg" alt="github code" /> Code</a>
      </div>
      <div class="supplementary widget" id="github-issues">
        <a href="http://issues.regex.uk"><img src="images/issue-opened.svg" alt="github issues" /> Issues</a>
      </div>
      <div class="widget-divider">&nbsp;</div>
      <div class="supplementary widget" id="blog-badge">
        <a href="http://blog.regex.uk">
          <img src="badges/blog.svg" alt="regex blog" />
        </a>
      </div>
      <div class="supplementary widget" id="hackage-badge">
        <a href="https://hackage.haskell.org/package/regex">
          <img src="badges/hackage.svg" alt="hackage version" />
        </a>
      </div>
      <div class="supplementary widget" id="build-status-badge">
        <a href="build-status">
          <img src="badges/build-status.svg" alt="build status" />
        </a>
      </div>
      <div class="supplementary widget" id="maintainers-contact">
        <a href="mailto:maintainers@regex.uk">
          <img src="badges/maintainers-contact.svg" alt="maintainers contact" />
        </a>
      </div>
      <div class="supplementary widget" id="feedback-contact">
        <a href="mailto:feedback@regex.uk">
          <img src="badges/feedback-contact.svg" alt="deedback contact" />
        </a>
      </div>
      <div class="supplementary widget twitter">
        <iframe style="width:162px; height:20px;" src="https://platform.twitter.com/widgets/follow_button.html?screen_name=hregex&amp;show_count=false">
        </iframe>
      </div>
    </div>
    <div id="content">
|]

pst_body_html :: LBS.ByteString
pst_body_html = [here|      </div>
    </div>
|] M.<> tracking
\end{code}


Task Lists
----------

\begin{code}
-- | replacement function to convert GFM task list line into HTML if we
-- aren't writing GFM (i.e.,  generating markdown for GitHub)
task_list :: MarkdownMode               -- ^ what flavour of md are we generating
          -> IORef Bool                 -- ^ will contain True iff we have already entered a task list
          -> Bool                       -- ^ true if this is a checjed line
          -> LineNo                     -- ^ line no of the replacement redex (unused)
          -> Match LBS.ByteString       -- ^ the matched task-list line
          -> RELocation                   -- ^ which match and capure (unused)
          -> Capture LBS.ByteString     -- ^ the capture weare replacing (unsuded)
          -> IO (Maybe LBS.ByteString)  -- ^ the replacement text, or Nothing to indicate no change to this line
task_list mmd rf chk _ mtch _ _ =
  case mmd of
    MM_github  -> return Nothing
    MM_hackage -> return $ Just $ "&nbsp;&nbsp;&nbsp;&nbsp;" M.<> cb M.<> "&nbsp;&nbsp;" M.<> itm M.<> "\n"
    MM_pandoc  -> do
      in_tl <- readIORef rf
      writeIORef rf True
      return $ tl_line in_tl chk
  where
    tl_line in_tl enbl = Just $ LBS.concat
      [ if in_tl then "" else "<ul class='contains-task-list'>\n"
      , "  <li class='task-list-item'>"
      , "<input type='checkbox' class='task-list-item-checkbox'"
      , if enbl then " checked=''" else ""
      , " disabled=''/>"
      , itm
      , "</li>"
      ]

    cb  = if chk then "&#x2612;" else "&#x2610;"

    itm = mtch !$$ [cp|itm|]

-- | replacement function used for 'other' lines -- terminate any task
-- list that was being generated
fin_task_list :: MarkdownMode               -- ^ what flavour of md are we generating
              -> IORef Bool                 -- ^ will contain True iff we have already entered a task list
              -> LineNo                     -- ^ line no of the replacement redex (unused)
              -> Match LBS.ByteString       -- ^ the matched task-list line
              -> RELocation                   -- ^ which match and capure (unused)
              -> Capture LBS.ByteString     -- ^ the capture weare replacing (unsuded)
              -> IO (Maybe LBS.ByteString)  -- ^ the replacement text, or Nothing to indicate no change to this line
fin_task_list mmd rf_t _ mtch _ _ =
  fmap ( M.<> matchSource mtch) <$> fin_task_list' mmd rf_t

-- | close any task list being processed, returning the closing text
-- as necessary
fin_task_list' :: MarkdownMode              -- ^ what flavour of md are we generating
               -> IORef Bool                -- ^ will contain True iff we have already entered a task list
               -> IO (Maybe LBS.ByteString) -- ^ task-list closure HTML, if task-list HTML needs closing
fin_task_list' mmd rf = do
  in_tl <- readIORef rf
  writeIORef rf False
  case mmd==MM_github || not in_tl of
    True  -> return Nothing
    False -> return $ Just $ "</ul>\n"
\end{code}


Literate Haskell Pages
----------------------

\begin{code}
pandoc_lhs :: String -> String -> String -> IO ()
pandoc_lhs title in_file = pandoc_lhs' title in_file in_file

pandoc_lhs' :: String -> String -> String -> String -> IO ()
pandoc_lhs' title repo_path in_file out_file = do
  lbsReadFile in_file >>= include_code_pp >>= LBS.writeFile int_file
  LBS.writeFile "tmp/metadata.markdown"  $
                    LBS.unlines
                      [ "---"
                      , "title: " M.<> LBS.pack title
                      , "---"
                      ]
  LBS.writeFile "tmp/bc.html" bc
  LBS.writeFile "tmp/ft.html" ft
  fmap (const ()) $
    SH.shelly $ SH.verbosely $
      SH.run "pandoc"
        [ "-f", "markdown+lhs+grid_tables"
        , "-t", "html5"
        , "-T", "regex"
        , "-s"
        , "-H", "lib/favicons.html"
        , "-B", "tmp/bc.html"
        , "-A", "tmp/ft.html"
        , "-c", "lib/lhs-styles.css"
        , "-c", "lib/bs.css"
        , "-o", T.pack out_file
        , "tmp/metadata.markdown"
        , T.pack int_file
        ]
  where
    bc = LBS.unlines
    --  [ "<div class='brandingdiv'>"
    --  , "  " M.<> branding
    --  , "</div>"
      [ "<div class='bcdiv'>"
      , "  <ol class='breadcrumb'>"
      , "    <li>" M.<> branding M.<> "</li>"
      , "    <li><a title='source file' href='"  M.<>
              repo_url M.<> "'>" M.<> (LBS.pack title) M.<> "</a></li>"
      , "</ol>"
      , "</div>"
      , "<div class='litcontent'>"
      ]

    ft = LBS.concat
      [ "</div>"
      ] M.<> tracking

    repo_url = LBS.concat
      [ "https://github.com/iconnect/regex/blob/master/"
      , LBS.pack repo_path
      ]

    int_file = "tmp/pandoc-int.lhs"
\end{code}


tweak_md
--------

\begin{code}
tweak_md :: MarkdownMode -> LBS.ByteString -> LBS.ByteString
tweak_md mm lbs = case mm of
    MM_github  -> lbs
    MM_pandoc  -> awk
      [ Template [ed|<https?://${rest}([^)]+)>///[${rest}]($0)|]
      ]
    MM_hackage -> awk
      [ Template [ed|<br/>$///\n|]
      ]
  where
    awk = fromMaybe oops . flip sed' lbs . Pipe

    -- runIdentity added to base in 4.9 only
    oops = error "tweak_md"
\end{code}


branding
--------

\begin{code}
branding :: LBS.ByteString
branding = [here|<a href="." style="font-family: Arial, 'Helvetica Neue', Helvetica, sans-serif;" id="branding">[<span style='color:red;'>re</span>|${<span style='color:red;'>gex</span>}(.*)|<span></span>]</a>|]
\end{code}


tracking
--------

\begin{code}
tracking :: LBS.ByteString
tracking = [here|    <script>
      (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
      (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
      m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
      })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

      ga('create', 'UA-92650418-1', 'auto');
      ga('send', 'pageview');

    </script>
|]
\end{code}


testing
-------

\begin{code}
test :: IO ()
test = do
  test_pp "re-prep doc" (prepare_tutorial Doc) "data/pp-test.lhs" "data/pp-result-doc.lhs"
  gm <- genMode
  test_pp "re-prep gen" (prepare_tutorial gm ) "data/pp-test.lhs" "data/pp-result-gen.lhs"
  putStrLn "tests passed"
\end{code}


\begin{code}
lbsReadFile :: FilePath -> IO LBS.ByteString
lbsReadFile fp = LBS.fromStrict <$> B.readFile fp
\end{code}
