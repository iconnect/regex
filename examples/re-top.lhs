top: Calculating league table variations for the Premier League
===============================================================

This program calculates top-n league tables for the Premier League based
on this [openfootball
data](https://github.com/cdornan/eng-england/tree/corrections).

The program has enough data to self-test but to generate any useful data
you will need to clone [this
repo](git@github.com:cdornan/eng-england.git) into the parent directory
and checkout the 'corrections' branch.

\begin{code}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE NoImplicitPrelude            #-}
{-# LANGUAGE RecordWildCards              #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE QuasiQuotes                  #-}

module Main(main) where

import qualified Control.Monad         as M
import           Data.Functor.Identity
import qualified Data.HashMap.Lazy     as HML
import qualified Data.List             as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Data.Time
import           Prelude.Compat
import qualified Shelly                as SH
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           TestKit
import           Text.RE.Summa
import qualified Text.RE.TDFA         as TDFA
import           Text.RE.TDFA.Text
import           Text.Read
\end{code}


The CLI parser yields a list of jobs to generate or check various
league tables.

\begin{code}
main :: IO ()
main = parseCLI >>= mapM_ job
\end{code}


Data Types
----------

A Job contains everything we need to generate a league table, and what
to do with it.

\begin{code}
data Job =
  Job
    { jobTitle  :: T.Text     -- ^ title for the table
    , jobSize   :: Maybe Int  -- ^ a full table or a top-n table
    , jobInputs :: [FilePath] -- ^ the files containing the game data
    , jobIsTest :: Bool       -- ^ are we testing o/p or writing it
    , jobIsHtml :: Bool       -- ^ are we generating HTML from markdown
    , jobOutput :: FilePath   -- ^ where is the output
    }
  deriving (Show)
\end{code}

A match result lists the data in the usual order:

    <home-team> <home-score> <away-score> <away-team>

\begin{code}
data Game = Game Team Int Int Team
  deriving (Eq,Ord,Read,Show)
\end{code}

A league table is a list of teams and their results with the ordering
on everything is arranged so that the list can be sorted with the
default `Ord` ordering to arrange the table according to PL conventions.

\begin{code}
newtype Table = Table { getTable :: [(Results,Team)] }
  deriving (Show)
\end{code}

Teams are just Text containing the names used by the openfootball data.

\begin{code}
type Team = T.Text
\end{code}

Results contain everything we need to generate a league table.

\begin{code}
data Results =
  Results
    { resultsGamesPlayed  :: Int
    , resultsGamesWon     :: Int
    , resultsGoalsFor     :: Int
    , resultsGoalsAgainst :: Int
    , resultsPointsScored :: Int
    }
  deriving (Show)
\end{code}

These vectors have expected zeros and sums.

\begin{code}
#if __GLASGOW_HASKELL__ >= 804
instance Semigroup Results where
  (<>) = mappend_r
#endif

instance Monoid Results where
  mempty  = mempty_r
  mappend = mappend_r
\end{code}


\begin{code}
mappend_r :: Results -> Results -> Results
mappend_r (Results gp1 gw1 gf1 ga1 ps1)
          (Results gp2 gw2 gf2 ga2 ps2) =
       Results (gp1+gp2)
               (gw1+gw2)
               (gf1+gf2)
               (ga1+ga2)
               (ps1+ps2)

mempty_r :: Results
mempty_r  = Results 0 0 0 0 0
\end{code}


PL results are ordered by (points,goal-difference,goals-scored).

\begin{code}
instance Ord Results where
  compare = comparing $ \Results{..} ->
      ( resultsPointsScored
      , resultsGoalsFor - resultsGoalsAgainst
      , resultsGoalsFor
      )

instance Eq Results where
  (==) x y = compare x y == EQ
\end{code}


Executing Jobs
--------------

\begin{code}
job :: Job -> IO ()
job jb = top jb <$> input jb >>= output jb
\end{code}

Compute a Table from Game data according to the jobSize parameter.
\begin{code}
top :: Job -> [Game] -> Table
top Job{..} mtchs = case jobSize of
    Nothing -> full_tbl
    Just n  -> aggregate $
      filterGames (topTeams n full_tbl) mtchs
  where
    full_tbl = aggregate mtchs
\end{code}

Calculate the top n teams in a table.

\begin{code}
topTeams :: Int -> Table -> [Team]
topTeams n = map snd . take n . getTable
\end{code}

Filter game data to only include games between the listed teams.

\begin{code}
filterGames :: [Team] -> [Game] -> [Game]
filterGames tms = filter $ \(Game h _ _ a) ->
    h `elem` tms && a `elem` tms
\end{code}

Aggregate game data into a table.

\begin{code}
aggregate :: [Game] -> Table
aggregate mtchs = sortResults $
    map homeResults mtchs ++ map awayResults mtchs
\end{code}

Generate the results for the home team from a Game result.

\begin{code}
homeResults :: Game -> (Results,Team)
homeResults (Game h hs as _) = flip (,) h
    Results
      { resultsGamesPlayed  = 1
      , resultsGamesWon     = wins   $ hs - as
      , resultsGoalsFor     = hs
      , resultsGoalsAgainst = as
      , resultsPointsScored = points $ hs - as
      }
\end{code}

Generate the results for the away team from a Game result.

\begin{code}
awayResults :: Game -> (Results,Team)
awayResults (Game _ hs as a) = flip (,) a
    Results
      { resultsGamesPlayed  = 1
      , resultsGamesWon     = wins   $ as - hs
      , resultsGoalsFor     = as
      , resultsGoalsAgainst = hs
      , resultsPointsScored = points $ as - hs
      }
\end{code}

Calculate the points accruing to a team from their goal difference
for a game.

\begin{code}
points :: Int -> Int
points gd = case compare gd 0 of
  LT -> 0
  EQ -> 1
  GT -> 3
\end{code}

Calculate the number of wins accruing to a team (0 or 1) from their goal
for a game.

\begin{code}
wins :: Int -> Int
wins gd = case gd>0 of
  True  -> 1
  False -> 0
\end{code}

Collate a list of results into a table.

\begin{code}
sortResults :: [(Results,Team)] -> Table
sortResults =
    Table
      . L.sortBy (flip $ comparing fst)
      . groupSortBy (comparing snd) grp
  where
    grp (r,t) ps = (r',t)
      where
        r' = mconcat $ r : map fst ps
\end{code}


input
-----

Parse openfootball data into Game data, eliminating duplicate results.
(For `g1` and `g2` to be recognised as a duplicate, it must be true that
`g1==g2`.)

\begin{code}
input :: Job -> IO [Game]
input Job{..} =
  groupSort const . parseGames . T.concat <$> mapM T.readFile jobInputs
\end{code}


parseGames
----------

The Game parser has three variants that should all be equivalent.

\begin{code}
parseGames :: T.Text -> [Game]
parseGames = case PrimParseGames of
  SimpleParseGames -> simpleParseGames
  FunParseGames    -> funParseGames
  PrimParseGames   -> primParseGames

data ParseGames = SimpleParseGames | FunParseGames | PrimParseGames
\end{code}


<h3>simpleParseGames</h3>

Here we apply the `gameEdit` `SearchReplace` editor to:

  1. recognise the lines that contain the match results data and

  2. transform the lines into Haskell `Game` format which can be
     parsed by `readText`.

The `edit` function is a simple specialisation of the `regex` `sed'`
function (defined below) that deletes every line in the file that edits
every line in the file according to the given `SearchReplace`, deleting
all other lies.


\begin{code}
simpleParseGames :: T.Text -> [Game]
simpleParseGames = map readText . T.lines . edit gameEdit
\end{code}

The `[ed|` ... `///` ... `|]` `SearchReplace` editors for recognizing
line containing matchresults and converting them to Haskell-format
`Game` data come in two variants that should be equivalent.

\begin{code}
gameEdit :: SearchReplace RE T.Text
gameEdit = case MacrosGameEdit of
  SimpleGameEdit -> simpleGameEdit
  MacrosGameEdit -> macrosGameEdit

data GameEdit = SimpleGameEdit | MacrosGameEdit
\end{code}


<h4>simpleGameEdit</h4>

\begin{code}
simpleGameEdit :: SearchReplace RE T.Text
simpleGameEdit =
  [ed|^ *${ht}([A-Za-z][a-zA-Z ]*[A-Za-z]) +${hs}([0-9]+)-${as}([0-9]+) +(\([^)]+\) *)?${at}([A-Za-z][a-zA-Z ]*[A-Za-z]) *(@.*)?$///Game "${ht}" ${hs} ${as} "${at}"|]
\end{code}


<h4>simpleGameEdit</h4>

\begin{code}
macrosGameEdit :: SearchReplace RE T.Text
macrosGameEdit =
  [ed_|^ *${ht}(@{team}) +${hs}([0-9]+)-${as}([0-9]+) +(\([0-9]+-[0-9]+\) *)?${at}(@{team}) *(@.*)?$///Game "${ht}" ${hs} ${as} "${at}"|] macs
\end{code}

\begin{code}
macs :: Macros RE
macs = makeMacros env

env :: MacroEnv
env = makeEnv [(,) "team" teamMacro] TDFA.preludeEnv
\end{code}

\begin{code}
teamMacro :: MacroEnv -> MacroID -> MacroDescriptor
teamMacro ev mid =
  runTests TDFA.regexType Just (map dup samples) ev mid
    MacroDescriptor
      { macroSource          = RegexSource "([a-zA-Z]+ ?)*[A-Za-z]" -- the RE to be substituted for the macro
      , macroSamples         = samples                              -- list of strings that should match the above macro RE
      , macroCounterSamples  = counter_samples                      -- list of strings that should **not** match the above macro RE
      , macroTestResults     = []                                   -- for bookkeeping
      , macroParser          = Nothing                              -- no parser needed for this one!
      , macroDescription     = "team names: alphabetic characters interspersed with spaces"
      }
  where
    samples =
        [ "Chelsea FC"
        , "West Bromwich Albion"
        , "AFC Bournemouth"
        , "F"
        , "AB"
        , "A B"
        , "AA B"
        ]

    counter_samples =
        [ "Arsenal FC "
        , " Liverpool FC"
        , "West Bromwich  Albion"
        , "F2"
        , ""
        ]

    dup x = (x,x)
\end{code}

\begin{code}
listMacros :: IO ()
listMacros = do
    hPutStr stderr $ formatMacroTable TDFA.regexType env
    ok <- testMacroEnv "macros" TDFA.regexType env
    M.when (not ok) $ exitWith $ ExitFailure 1
\end{code}


<h3>funParseGames</h3>

Here we use the `regex` `grepFilter` to extract all of the lines that
match our `rex` RE for detecting match-result data and assemble the
`Game` data directly by extracting the `ht`, `hs`, `as` and `at`
fields from the matched result.

\begin{code}
funParseGames :: T.Text -> [Game]
funParseGames txt =
    [ Game (           mtch !$$ [cp|ht|])
           (readText $ mtch !$$ [cp|hs|])
           (readText $ mtch !$$ [cp|as|])
           (           mtch !$$ [cp|at|])
        | Line{..} <- grepFilter rex txt
        , [mtch]   <- [allMatches getLineMatches]
        ]
\end{code}

The RE for merely recognising lines that contain match results in the
input data come in two variants. We either extract the RE from the above
`SearchReplace` template or rebuild the `[re|` ... `]`. (They should of
course be equivalent.)

\begin{code}
rex :: RE
rex = case Direct of
  Direct  ->
    [re_|^ *${ht}(@{team}) +${hs}([0-9]+)-${as}([0-9]+) +(\([0-9]+-[0-9]+\) *)?${at}(@{team}) *(@.*)?$|] macs
  Recycle ->
    getSearch gameEdit

data REX = Direct | Recycle
\end{code}


<h3>primParseGames</h3>

This variant of `funParseGames` uses `T.lines` and `?=~` instead of
`grepFilter`.

\begin{code}
primParseGames :: T.Text -> [Game]
primParseGames txt =
    [ Game (           mtch !$$ [cp|ht|])
           (readText $ mtch !$$ [cp|hs|])
           (readText $ mtch !$$ [cp|as|])
           (           mtch !$$ [cp|at|])
        | mtch <- map (?=~ rex) $ T.lines txt
        , matched mtch
        ]
\end{code}


output
------

Write out/test the Table according to the Job output parameters.

\begin{code}
output :: Job -> Table -> IO ()
output jb@Job{..} tbl = case jobIsTest of
    True  -> test_it =<< formatTable jb tbl
    False -> case jobOutput of
      "-" -> T.putStr       =<< formatTable jb tbl
      fp  -> T.writeFile fp =<< formatTable jb tbl
  where
    test_it txt = do
      txt' <- T.readFile jobOutput
      case txt == txt' of
        True  -> putStrLn "OK"
        False -> do
          putStrLn "Test Failed"
          exitWith $ ExitFailure 1
\end{code}

Generate the markdown for a table and optionally use Pandoc to generate
the Html.

\begin{code}
formatTable :: Job -> Table -> IO T.Text
formatTable Job{..} (Table ps) = to_html $ T.unlines $
    [ "# " <> jobTitle
    , ""
    , mk_row header_row
    , mk_row divider_row
    ] ++ map mk_row (zipWith gen_row [1..] ps)
  where
    divider_row = map (T.map (const '-')) header_row
    header_row  = map column_header [minBound..maxBound]

    mk_row :: [T.Text] -> T.Text
    mk_row = T.intercalate "|"

    gen_row :: Int -> (Results,Team) -> [T.Text]
    gen_row i (r,t) = map (gen_field i t r) [minBound..maxBound]

    gen_field :: Int -> Team -> Results -> Col -> T.Text
    gen_field i tm Results{..} col = lj $ case col of
        Position  -> showText i
        Club      -> tm
        Played    -> showText   resultsGamesPlayed
        Won       -> showText   resultsGamesWon
        Drawn     -> showText   games_drawn
        Lost      -> showText $ resultsGamesPlayed -
                            (resultsGamesWon + games_drawn)
        GF        -> showText   resultsGoalsFor
        GA        -> showText   resultsGoalsAgainst
        GD        -> showText $ resultsGoalsFor -
                                        resultsGoalsAgainst
        Points    -> showText   resultsPointsScored
      where
        lj = T.justifyLeft wd ' '
        wd = T.length $ column_header col

        games_drawn = resultsPointsScored - win_points
        win_points  = resultsGamesWon * 3

    to_html = case jobIsHtml of
      True  -> pandoc jobTitle
      False -> return

data Col
  = Position
  | Club
  | Played
  | Won
  | Drawn
  | Lost
  | GF
  | GA
  | GD
  | Points
  deriving (Bounded,Enum,Show)

column_header :: Col -> T.Text
column_header col = case col of
  Position  -> "Pos"
  Club      -> "Club                 "
  _         -> T.justifyLeft 7 ' ' $ showText col
\end{code}


parseCLI
--------

The command line parser generates a list of league-table
generating/testing jobs for execution by the above `job`
action. Non-league-table-generating CLI commands like
`macros` for listing our RE table macros just do their
thing and return an empty list of jobs.

\begin{code}
parseCLI :: IO [Job]
parseCLI = do
  args <- getArgs
  case args of
    []                 -> listMacros  >> testJob      -- test with canned vectors
    ["test"]           -> listMacros  >> testJob      --    "
    ["setup-test"]     ->                setupTestJob -- setup the test data, generating the golden data
    ["update"]         -> updateIndex >> updateJobs   -- update the website with latest data
    ["table",pth]      -> discover pth   Nothing      -- write out a full league table
    ["table",pth,sz_s]
      | Just sz <- readMaybe sz_s
                       -> discover pth $ Just sz      -- write out a bounded league table
    ["macros"]         -> listMacros  >> return []    -- list the RE macros we are using to parse the data
    _                  -> do                          -- generate the usage message and fail
        pn <- getProgName
        hPutStr stderr $ prog pn
          [ "[test]"
          , "setup-test"
          , "update"
          , "table   <path> [<size>]"
          ]
        exitWith $ ExitFailure 1
  where
    prog pn as = unlines $ zipWith prog_ (pn : repeat pn') as
      where
        pn'       = map (const ' ') pn

        prog_ p a = unwords [p,a]

testJob, setupTestJob :: IO [Job]
testJob      = testJob_ True
setupTestJob = testJob_ False

testJob_ :: Bool -> IO [Job]
testJob_ is_t = do
  createDirectoryIfMissing True "data"
  return
    [ Job
        { jobTitle  = "Premier League 2015-16: Top 7"
        , jobSize   = Just 7
        , jobInputs =
            [ "data/2015-16-premierleague.txt"
            ]
        , jobIsTest = is_t
        , jobIsHtml = False
        , jobOutput = "data/league-table.md"
        }
    ]

updateJobs :: IO [Job]
updateJobs = do
    dy <- utctDay <$> getCurrentTime
    return $ concat $ map (mk dy) updateJobSpecs
  where
    mk dy js@JobSpec{..} =
        [ Job
            { jobTitle  = T.unwords $
                [ "Premier League " <> T.pack jsSeason
                , maybe "" (\n->"Top "<>showText n<>" ") jsSize
                , "["<>showText dy<>"]"
                ]
            , jobSize   = jsSize
            , jobInputs =
                [ mkPath jsSeason "1-premierleague-i.txt"
                , mkPath jsSeason "1-premierleague-ii.txt"
                ]
            , jobIsTest = False
            , jobIsHtml = is_html
            , jobOutput = leagueTablesDir </> tableFile js is_html
            }
        | is_html <- [True,False]
        ]

updateIndex :: IO ()
updateIndex = pandoc title toc >>= T.writeFile index_file
  where
    toc = T.unlines $
      [ "# " <> title
      , ""
      , mk_row ["Season", "Top-N", "Html", "Text"]
      , mk_row ["------", "-----", "----", "----"]
      ] ++
      [ mk_row
          [ T.pack jsSeason
          , maybe "all" showText jsSize
          , lk "HTML" $ T.pack $ tableFile js True
          , lk "Text" $ T.pack $ tableFile js False
          ]
        | js@JobSpec{..} <- updateJobSpecs
        ]

    title      = "The League Tables"
    index_file = leagueTablesDir </> "index.html"

    mk_row     = T.intercalate "|"

    lk lab url = "["<>lab<>"]("<>url<>")"

tableFile :: JobSpec -> Bool -> FilePath
tableFile JobSpec{..} is_html = jsSeason ++ "-" ++ sze <.> ext
  where
    sze = maybe "all" (("top-"++).show) jsSize
    ext = if is_html then "html" else "txt"

leagueTablesDir :: FilePath
leagueTablesDir = "docs/league-tables"

updateJobSpecs :: [JobSpec]
updateJobSpecs =
  [ JobSpec sn mb
    | sn <- ["2016-17","2015-16"]
    , mb <- Nothing : map Just [6..10]
    ]

data JobSpec =
  JobSpec
    { jsSeason :: String
    , jsSize   :: Maybe Int
    }
  deriving (Show)

discover :: FilePath -> Maybe Int -> IO [Job]
discover fp mb = do
  inps <- dscvr candidates
  return
    [ Job
        { jobTitle  = maybe fp_t mk_ttl $
              matchedText $ fp_t ?=~ [re|[0-9]{4}-[0-9]{2}|]
        , jobSize   = mb
        , jobInputs = inps
        , jobIsTest = False
        , jobIsHtml = False
        , jobOutput = "-"
        }
    ]
  where
    fp_t            = T.pack fp

    mk_ttl ssn      = "Premier League " <> ssn

    dscvr []        = error $ fp ++ ": no data found"
    dscvr (fps:cds) = do
      ok <- and <$> mapM doesFileExist fps
      case ok of
        True  -> return fps
        False -> dscvr cds

    candidates =
      [ [ fp
        ]
      , [ data_dir </> fp </> "1-premierleague.txt"
        ]
      , [ data_dir </> fp </> "1-premierleague-i.txt"
        , data_dir </> fp </> "1-premierleague-ii.txt"
        ]
      ]

mkPath :: String -> String -> FilePath
mkPath ssn hlf = data_dir </> ssn </> hlf

data_dir :: FilePath
data_dir = "../eng-england"
\end{code}


Pandoc
------

Use Pandoc to generate a an Html file from a title and markdown text.

\begin{code}
pandoc :: T.Text -> T.Text -> IO T.Text
pandoc title txt = do
    T.writeFile inp_file   txt
    T.writeFile mda_file $ T.unlines
          [ "---"
          , "title: "<>title
          , "---"
          ]
    fmap (const ()) $
      SH.shelly $ SH.verbosely $
        SH.run "pandoc"
          [ "-f", "markdown+grid_tables"
          , "-t", "html5"
          , "-T", "regex"
          , "-s"
          , "-c", "bs.css"
          , "-c", "styles.css"
          , "-c", "tabular.css"
          , "-o", T.pack out_file
          , T.pack mda_file
          , T.pack inp_file
          ]
    T.readFile out_file
  where
    mda_file = "tmp/metadata.markdown"
    inp_file = "tmp/pandoc-inp.md"
    out_file = "tmp/pandoc-out.html"
\end{code}


Helpers
-------

The general helpers.

<h3>edit</h3>

The `edit` function is a simple specialisation of the `regex`
`sed'` function (defined below) that deletes every line in the file
that edits every line in the file according to the given `SearchReplace`
template, deleting all other lines. (It should probably be added to
regex.)

\begin{code}
edit :: SearchReplace RE T.Text -> T.Text -> T.Text
edit sr txt = runIdentity $ flip sed' txt $
    Select
      [ Template sr
      , LineEdit [re|.*|] $ \_ _ -> return Delete
      ]
\end{code}

<h3>makeMacros and makeEnv</h3>

Construct a Macros table for compiling REs from a MacroEnv. (Something
similar  should probably be added to regex.)

\begin{code}
makeMacros :: MacroEnv -> Macros RE
makeMacros ev = runIdentity $
    mkMacros mk TDFA.regexType ExclCaptures ev
  where
    mk   = maybe oops Identity .
                TDFA.compileRegexWithOptions TDFA.noPreludeREOptions

    oops = error "makeMacros: unexpected RE compilation error"
\end{code}

Construct a a `MacroEnv` from an association list of `MacroId` and
`MacroDescriptior` constructor functions and the base `MacroEnv`
(the macros that can be used inside the macros). (Something
similar  should probably be added to regex.)

\begin{code}
makeEnv :: [(MacroID,MacroEnv -> MacroID -> MacroDescriptor)]
        -> MacroEnv
        -> MacroEnv
makeEnv al ev0 = ev
  where
    ev = ev0 `HML.union` HML.fromList
                  [ (mid, mk ev mid) | (mid,mk) <- al ]
\end{code}


<h3>showText and readText</h3>

Variants of the standard functions that operate over `Text`.

\begin{code}
showText :: Show a => a -> T.Text
showText = T.pack . show

readText :: Read a => T.Text -> a
readText = fromMaybe (error "readText") . readMaybe . T.unpack
\end{code}
