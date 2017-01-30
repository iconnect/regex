\begin{code}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Text.RE.TestBench
  ( MacroID(..)
  , RegexType(..)
  , MacroEnv
  , WithCaptures(..)
  , MacroDescriptor(..)
  , TestResult(..)
  , RegexSource(..)
  , FunctionID(..)
  , mkMacros
  , testMacroEnv
  , badMacros
  , runTests
  , runTests'
  , formatMacroTable
  , dumpMacroTable
  , formatMacroSummary
  , formatMacroSources
  , formatMacroSource
  , testMacroDescriptors
-- , regexSource
  ) where

import           Data.Array
import           Control.Applicative
import qualified Data.HashMap.Lazy              as HML
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.String
import           Text.Printf
import           Text.RE.Capture
import           Text.RE.Options
import           Text.RE.Replace
import qualified Text.Regex.PCRE                as PCRE
import qualified Text.Regex.TDFA                as TDFA
\end{code}

Types
-----

\begin{code}
-- | what kind of back end will be compiling the RE
data RegexType
  = TDFA    -- the TDFA back end
  | PCRE    -- the PCRE back end
  deriving (Bounded,Enum,Eq,Ord,Show)

-- | do we need the captures in the RE or whould they be stripped out
-- where possible
data WithCaptures
  = InclCaptures
  | ExclCaptures
  deriving (Eq,Ord,Show)

-- | each macro can reference others, the whole environment being
-- required for each macro, so we use a Lazy HashMap
type MacroEnv = HML.HashMap MacroID MacroDescriptor

-- | describes a macro, giving the text of the RE and a si=ummary
-- description
data MacroDescriptor =
  MacroDescriptor
    { _md_source          :: !RegexSource         -- ^ the RE
    , _md_samples         :: ![String]            -- ^ some sample matches
    , _md_counter_samples :: ![String]            -- ^ some sample non-matches
    , _md_test_results    :: ![TestResult]        -- ^ validation test results
    , _md_parser          :: !(Maybe FunctionID)  -- ^ WA, the parser function
    , _md_description     :: !String              -- ^ summary comment
    }
  deriving (Show)

-- | list of failures on a validation run
newtype TestResult =
  TestResult { _TestResult :: String }
  deriving (IsString,Show)

-- | a RE that should work for POSIX and PCRE with open brackets ('(')
-- represented as follows:
--    \(    mere symbol
--    (?:   used for grouping only, not for captures
--    (}:   used for captures only, not for grouping
--    (]:   used for captures and grouping
--    (     do not modify
newtype RegexSource =
    RegexSource { _RegexSource :: String }
  deriving (IsString,Show)

-- | name of the Haskell parser function for parsing the text matched
-- by a macro
newtype FunctionID =
    FunctionID { _FunctionID :: String }
  deriving (IsString,Show)

-- | we are only interested in the open parentheses used for
-- grouping and/or capturing; if neither grouping or capturing then
-- there is no initial '(' or '(?:', just the suffic text
data REToken =
  REToken
    { _ret_prefix    :: String  -- ^ following text optional ( or (?:
    , _ret_fixed     :: Bool    -- ^ a '(' that is not safe to modify
    , _ret_grouping  :: Bool    -- ^ is this a grouping group
    , _ret_capturing :: Bool    -- ^ is this a capturing group
    }
  deriving (Show)
\end{code}


mkMacros
--------

\begin{code}
mkMacros :: (Monad m,Functor m)
         => (String->m r)
         -> RegexType
         -> WithCaptures
         -> MacroEnv
         -> m (Macros r)
mkMacros prs rty wc env =
    HML.fromList <$> mapM (uncurry mk) (HML.toList env)
  where
    mk mid md = (,) mid <$> prs (mdRegexSource rty wc env md)
\end{code}


testMacroEnv, badMacros
-----------------------

\begin{code}
testMacroEnv :: String -> RegexType -> MacroEnv -> IO Bool
testMacroEnv lab rty m_env = case badMacros m_env of
  []    -> return True
  fails -> do
    putStrLn $ lab' ++ " has failing tests for these macros: "
    putStr   $ unlines $ [ "  "++_MacroID mid | mid<-fails ]
    putStrLn $ "The whole table:"
    putStrLn $ "========================================================"
    putStr   $ formatMacroTable rty m_env
    putStrLn $ "========================================================"
    return False
  where
    lab' = lab ++ " [" ++ show rty ++"]"

badMacros :: MacroEnv -> [MacroID]
badMacros m_env =
  [ mid
      | (mid,MacroDescriptor{..}) <- HML.toList m_env
      , not $ null _md_test_results
      ]

runTests :: (Eq a,Show a)
         => RegexType
         -> (String->Maybe a)
         -> [(String,a)]
         -> MacroEnv
         -> MacroID
         -> MacroDescriptor
         -> MacroDescriptor
runTests rty parser = runTests' rty parser'
  where
    parser' caps = fmap capturedText (matchCapture caps) >>= parser

runTests' :: (Eq a,Show a)
          => RegexType
          -> (Match String->Maybe a)
          -> [(String,a)]
          -> MacroEnv
          -> MacroID
          -> MacroDescriptor
          -> MacroDescriptor
runTests' rty parser vector env mid md@MacroDescriptor{..} =
    md { _md_test_results = test_results }
  where
    test_results = concat
      [ concat $ map test     vector
      , concat $ map test_neg _md_counter_samples
      ]

    test (src,x) = test'     mid rty parser x $ match_ src env md

    test_neg src = test_neg' mid rty parser   $ match_ src env md

    match_ = case rty of
      TDFA -> match_tdfa
      PCRE -> match_pcre
\end{code}


dumpMacroTable, formatMacroTable, formatMacroSummary, formatMacroSources, formatMacroSource
-------------------------------------------------------------------------------------------

\begin{code}
dumpMacroTable :: String -> RegexType -> MacroEnv -> IO ()
dumpMacroTable lab rty m_env = do
    writeFile fp_t $ formatMacroTable   rty              m_env
    writeFile fp_s $ formatMacroSources rty ExclCaptures m_env
  where
    fp_t = "tables/" ++ lab ++ "-" ++ show rty ++ ".md"
    fp_s = "tables/" ++ lab ++ "-" ++ show rty ++ ".txt"
\end{code}

\begin{code}
formatMacroTable :: RegexType -> MacroEnv -> String
formatMacroTable rty env = unlines $
  format_table macro_table_hdr
    [ macro_table_row rty mid md
        | (mid,md) <- sortBy (comparing fst) $ HML.toList env
        ]
\end{code}

\begin{code}
formatMacroSummary :: RegexType -> MacroEnv -> MacroID -> String
formatMacroSummary rty env mid = maybe oops prep $ HML.lookup mid env
  where
    prep :: MacroDescriptor -> String
    prep md = unlines $ concat $ map (fmt md) [minBound..maxBound]

    fmt :: MacroDescriptor -> Col -> [String]
    fmt md c =
        [ printf "%-15s : %s" (present_col c) ini
        ] ++ map ("      "++) lns
      where
        (ini,lns) = case macro_attribute rty mid md c of
          []   -> (,) "" []
          [ln] -> (,) ln []
          lns_ -> (,) "" lns_

    oops = error $ _MacroID mid ++ ": macro not defined in this environment"
\end{code}

\begin{code}
formatMacroSources :: RegexType
                   -> WithCaptures
                   -> MacroEnv
                   -> String
formatMacroSources rty wc env = unlines $
    [ printf "%-20s : %s" (_MacroID mid) $ formatMacroSource rty wc env mid
        | mid <- sort $ HML.keys env
        ]
\end{code}

\begin{code}
formatMacroSource :: RegexType
                  -> WithCaptures
                  -> MacroEnv
                  -> MacroID
                  -> String
formatMacroSource rty wc env mid =
    mdRegexSource rty wc env $ fromMaybe oops $ HML.lookup mid env
  where
    oops = error $ "formatMacroSource: not found: " ++ _MacroID mid
\end{code}


testMacroDescriptors, regexSource
---------------------------------

\begin{code}
testMacroDescriptors :: [MacroDescriptor] -> [TestResult]
testMacroDescriptors = concat . map _md_test_results

regexSource :: RegexType -> WithCaptures -> RegexSource -> String
regexSource rty wc = format_tokens rty wc . scan_re
\end{code}


Formatting helpers
------------------

\begin{code}
type TableRow = Array Col [String]

data Col
  = C_name
  | C_caps
  | C_regex
  | C_examples
  | C_anti_examples
  | C_fails
  | C_parser
  | C_comment
  deriving (Ix,Bounded,Enum,Ord,Eq,Show)

present_col :: Col -> String
present_col = map tr . drop 2 . show
  where
    tr '_' = '-'
    tr c   = c

macro_table_hdr :: TableRow
macro_table_hdr = listArray (minBound,maxBound)
  [ [present_col c]
    | c<-[minBound..maxBound]
    ]

macro_table_row :: RegexType -> MacroID -> MacroDescriptor -> TableRow
macro_table_row rty mid md =
    listArray (minBound,maxBound) $
      map (macro_attribute rty mid md) [minBound..maxBound]

macro_attribute :: RegexType
                -> MacroID
                -> MacroDescriptor
                -> Col
                -> [String]
macro_attribute rty mid MacroDescriptor{..} c =
    case c of
      C_name          -> [_MacroID mid]
      C_caps          -> [show $ min_captures rty $ scan_re _md_source]
      C_regex         -> [regexSource rty ExclCaptures _md_source]
      C_examples      -> _md_samples
      C_anti_examples -> _md_counter_samples
      C_fails         -> map _TestResult _md_test_results
      C_parser        -> [maybe "-" _FunctionID _md_parser]
      C_comment       -> [_md_description]

format_table :: TableRow -> [TableRow] -> [String]
format_table hdr rows0 = concat
    [ format_row cws hdr'
    , format_row cws dsh
    , concat $ map (format_row cws) rows
    ]
  where
    dsh  = listArray (minBound,maxBound)
              [ [replicate n '-'] | n<-elems cws ]

    hdr' = hdr // [(,) C_regex $ [take n $ concat $ repeat "regex="] ]
      where
        n = min 29 $ cws!C_regex

    cws  = widths $ hdr : rows

    rows = map wrap_row rows0

field_width :: Int
field_width = 40

wrap_row :: TableRow -> TableRow
wrap_row = fmap $ concat . map f
  where
    f, g :: String -> [String]

    f cts = (ini ++ ['\\' | not (null rst)]) : g rst
      where
        (ini,rst) = splitAt (1+field_width) cts

    g ""  = []
    g cts = ('\\' : ini ++ ['\\' | not (null rst)]) : g rst
      where
        (ini,rst) = splitAt field_width cts


widths :: [TableRow] -> Array Col Int
widths rows = listArray (minBound,maxBound)
  [ maximum $ concat [ map length $ row!c | row<-rows ]
    | c<-[minBound..maxBound]
    ]

format_row :: Array Col Int -> TableRow -> [String]
format_row cw_arr row =
  [ ("|"++) $ intercalate "|"
      [ field cw_arr row c i | c<-[minBound..maxBound] ]
    | i <- [0..depth-1]
    ]
  where
    depth = maximum [ length $ row!c | c<-[minBound..maxBound] ]

field :: Array Col Int -> TableRow -> Col -> Int -> String
field cws row c i = ljust (cws!c) $ sel i $ row!c

sel :: Int -> [String] -> String
sel i ss = case drop i ss of
  []  -> ""
  s:_ -> s

ljust :: Int -> String -> String
ljust w s = s ++ replicate n ' '
  where
    n = max 0 $ w - length s

min_captures :: RegexType -> [REToken] -> Int
min_captures rty rets = length
  [ ()
    | REToken{..}<-rets
    , _ret_fixed || (_ret_grouping && rty==TDFA)
    ]
\end{code}


Formatting tokens
-----------------

\begin{code}
format_tokens :: RegexType -> WithCaptures -> [REToken] -> String
format_tokens rty wc = foldr f ""
  where
    f REToken{..} rst = _ret_prefix ++ bra ++ xket rst
      where
        bra = case _ret_fixed of
          True  -> "("
          False ->
            case (,) _ret_grouping (_ret_capturing && wc==InclCaptures) of
              (False,False) -> ""
              (True ,False) -> if rty==PCRE then "(?:" else "("
              (False,True ) -> "("
              (True ,True ) -> "("

        xket =
          case not _ret_grouping && _ret_capturing && wc==ExclCaptures of
            True  -> delete_ket 0
            False -> id

delete_ket :: Int -> String -> String
delete_ket _ "" = error "delete_ket: end of input"
delete_ket n (c:t) = case c of
  '\\' -> case t of
    ""    -> error "delete_ket: end of input"
    c':t' -> c : c' : delete_ket n t'
  ')'  -> case n of
    0  -> t
    _  -> c : delete_ket (n-1) t
  '('  -> c : delete_ket (n+1) t
  _    -> c : delete_ket  n    t
\end{code}


scan_re
-------

\begin{code}
scan_re :: RegexSource -> [REToken]
scan_re (RegexSource src0) = loop src0
  where
    loop ""  = []
    loop src =
        case rst of
          '\\':t -> case t of
              ""    -> REToken (ini++['\\'])    False False False : []
              c':t' -> REToken (ini++['\\',c']) False False False : loop t'
          '(' :t -> case t of
            c:':':t'
              | c=='?'  -> REToken  ini False True  False : loop t'
              | c=='}'  -> REToken  ini False False True  : loop t'
              | c==']'  -> REToken  ini False True  True  : loop t'
            _           -> REToken  ini True  True  True  : loop t
          _ -> [REToken src False False False]
      where
        (ini,rst) = break chk src

        chk '\\'  = True
        chk '('   = True
        chk _     = False
\end{code}


scan_re
-------

\begin{code}
match_tdfa :: String -> MacroEnv -> MacroDescriptor -> Matches String
match_tdfa txt env md = txt TDFA.=~ mdRegexSource TDFA ExclCaptures env md

match_pcre :: String -> MacroEnv -> MacroDescriptor -> Matches String
match_pcre txt env md = txt PCRE.=~ mdRegexSource PCRE ExclCaptures env md
\end{code}


mdRegexSource
-------------

\begin{code}
mdRegexSource :: RegexType
              -> WithCaptures
              -> MacroEnv
              -> MacroDescriptor
              -> String
mdRegexSource rty wc env md =
    expandMacros' lu $ regexSource rty wc $ _md_source md
  where
    lu  = fmap (regexSource rty wc . _md_source) .
            flip HML.lookup env
\end{code}


test', test_neg'
----------------

\begin{code}
test' :: (Eq a,Show a)
      => MacroID
      -> RegexType
      -> (Match String->Maybe a)
      -> a
      -> Matches String
      -> [TestResult]
test' mid rty prs x Matches{..} = either (:[]) (const []) $ do
    cs <- case allMatches of
      [cs] -> return cs
      _    -> oops "RE failed to parse"
    mtx <- case matchCapture cs of
      Nothing -> oops $ "RE parse failure: " ++ show cs
      Just c  -> return $ capturedText c
    case mtx == matchesSource of
      True  -> return ()
      False -> oops "RE failed to match the whole text"
    x' <- case prs cs of
      Nothing -> oops "matched text failed to parse"
      Just x' -> return x'
    case x'==x of
      True  -> return ()
      False -> oops "parser failed to yield the expected result"
  where
    oops = Left . test_diagnostic mid False rty matchesSource

test_neg' :: MacroID
          -> RegexType
          -> (Match String->Maybe a)
          -> Matches String
          -> [TestResult]
test_neg' mid rty prs Matches{..} = either id (const []) $ do
    case allMatches of
      [] -> return ()
      cz -> case ms of
          [] -> return ()
          _  -> Left [oops "RE parse succeeded"]
        where
          ms =
            [ ()
              | cs     <- cz
              , Just c <- [matchCapture cs]
              , let t = capturedText c
              , t == matchesSource
              , isJust $ prs cs
              ]

  where
    oops = test_diagnostic mid True rty matchesSource

test_diagnostic :: MacroID
                -> Bool
                -> RegexType
                -> String
                -> String
                -> TestResult
test_diagnostic mid is_neg rty tst msg =
    TestResult $
      printf "%-20s [%s %s] : %s (%s)" mid_s neg_s rty_s msg tst
  where
    mid_s = _MacroID mid
    neg_s = if is_neg then "-ve" else "+ve" :: String
    rty_s = show rty
\end{code}
