\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.ZeInternals.TestBench
  ( MacroID(..)
  , RegexType
  , mkTDFA
  , mkPCRE
  , isTDFA
  , isPCRE
  , presentRegexType
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
  , formatMacroSummary
  , formatMacroSources
  , formatMacroSource
  , testMacroDescriptors
  , mdRegexSource
  ) where

import           Data.Array
import qualified Data.HashMap.Lazy              as HML
import qualified Data.List                      as L
import           Data.Maybe
import           Data.Ord
import           Data.String
import           Prelude.Compat
import           Text.Printf
import           Text.RE.REOptions
import           Text.RE.ZeInternals.Replace
import           Text.RE.ZeInternals.Types.Capture
import           Text.RE.ZeInternals.Types.Match
import           Text.RE.ZeInternals.Types.Matches
\end{code}

Types
-----

\begin{code}

type TestBenchMatcher =
    String -> MacroEnv -> MacroDescriptor -> Matches String

-- | what flavour of regex are we dealing with
data RegexType
  = TDFA TestBenchMatcher
  | PCRE TestBenchMatcher

-- | test RegexType for TDFA/PCREness
isTDFA, isPCRE :: RegexType -> Bool

isTDFA (TDFA _) = True
isTDFA (PCRE _) = False

isPCRE (TDFA _) = False
isPCRE (PCRE _) = True

mkTDFA, mkPCRE :: TestBenchMatcher -> RegexType
mkTDFA = TDFA
mkPCRE = PCRE

presentRegexType :: RegexType -> String
presentRegexType (TDFA _) = "TDFA"
presentRegexType (PCRE _) = "PCRE"

instance Show RegexType where
  show (TDFA _) = "TDFA <function>"
  show (PCRE _) = "PCRE <function>"

-- | do we need the captures in the RE or whould they be stripped out
-- where possible
data WithCaptures
  = InclCaptures      -- ^ include all captures
  | ExclCaptures      -- ^ remove captures where possible
  deriving (Eq,Ord,Show)

-- | each macro can reference others, the whole environment being
-- required for each macro, so we use a Lazy HashMap
type MacroEnv = HML.HashMap MacroID MacroDescriptor

-- | describes a macro, giving the text of the RE and a si=ummary
-- description
data MacroDescriptor =
  MacroDescriptor
    { macroSource         :: !RegexSource         -- ^ the RE
    , macroSamples        :: ![String]            -- ^ some sample matches
    , macroCounterSamples :: ![String]            -- ^ some sample non-matches
    , macroTestResults    :: ![TestResult]        -- ^ validation test results
    , macroParser         :: !(Maybe FunctionID)  -- ^ WA, the parser function
    , macroDescription    :: !String              -- ^ summary comment
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
-- | construct a macro table suitable for use with the RE compilers
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
-- | test that a MacroEnv is passing all of its built-in tests
testMacroEnv :: String -> RegexType -> MacroEnv -> IO Bool
testMacroEnv lab rty m_env = case badMacros m_env of
  []    -> return True
  fails -> do
    putStrLn $ lab' ++ " has failing tests for these macros: "
    putStr   $ unlines $ [ "  "++getMacroID mid | mid<-fails ]
    putStrLn $ "The whole table:"
    putStrLn $ "========================================================"
    putStr   $ formatMacroTable rty m_env
    putStrLn $ "========================================================"
    return False
  where
    lab' = lab ++ " [" ++ presentRegexType rty ++"]"

badMacros :: MacroEnv -> [MacroID]
badMacros m_env =
  [ mid
      | (mid,MacroDescriptor{..}) <- HML.toList m_env
      , not $ null macroTestResults
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
    md { macroTestResults = test_results }
  where
    test_results = concat
      [ concat $ map test     vector
      , concat $ map test_neg macroCounterSamples
      ]

    test (src,x) = test'     mid rty parser x $ match_ src env md

    test_neg src = test_neg' mid rty parser   $ match_ src env md

    match_ = case rty of
      TDFA tbmf -> tbmf
      PCRE tbmf -> tbmf
\end{code}


formatMacroTable, formatMacroSummary, formatMacroSources, formatMacroSource
---------------------------------------------------------------------------

\begin{code}
-- | format a macros table as a markdown table
formatMacroTable :: RegexType -> MacroEnv -> String
formatMacroTable rty env = unlines $
  format_table macro_table_hdr
    [ macro_table_row rty mid md
        | (mid,md) <- L.sortBy (comparing fst) $ HML.toList env
        ]
\end{code}

\begin{code}
-- | generate a plain text summary of a macro
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

    oops = error $ getMacroID mid ++ ": macro not defined in this environment"
\end{code}

\begin{code}
-- | list the source REs for each macro in plain text
formatMacroSources :: RegexType
                   -> WithCaptures
                   -> MacroEnv
                   -> String
formatMacroSources rty wc env = unlines $
    [ printf "%-20s : %s" (getMacroID mid) $ formatMacroSource rty wc env mid
        | mid <- L.sort $ HML.keys env
        ]
\end{code}

\begin{code}
-- | list the source of a single macro in plain text
formatMacroSource :: RegexType
                  -> WithCaptures
                  -> MacroEnv
                  -> MacroID
                  -> String
formatMacroSource rty wc env mid =
    mdRegexSource rty wc env $ fromMaybe oops $ HML.lookup mid env
  where
    oops = error $ "formatMacroSource: not found: " ++ getMacroID mid
\end{code}


testMacroDescriptors, regexSource
---------------------------------

\begin{code}
testMacroDescriptors :: [MacroDescriptor] -> [TestResult]
testMacroDescriptors = concat . map macroTestResults

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
      C_name          -> [getMacroID mid]
      C_caps          -> [show $ min_captures rty $ scan_re macroSource]
      C_regex         -> [regexSource rty ExclCaptures macroSource]
      C_examples      -> macroSamples
      C_anti_examples -> macroCounterSamples
      C_fails         -> map _TestResult macroTestResults
      C_parser        -> [maybe "-" _FunctionID macroParser]
      C_comment       -> [macroDescription]

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
  [ ("|"++) $ L.intercalate "|"
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
    , _ret_fixed || (_ret_grouping && isTDFA rty)
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
              (True ,False) -> if isPCRE rty then "(?:" else "("
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


mdRegexSource
-------------

\begin{code}
mdRegexSource :: RegexType
              -> WithCaptures
              -> MacroEnv
              -> MacroDescriptor
              -> String
mdRegexSource rty wc env md =
    expandMacros' lu $ regexSource rty wc $ macroSource md
  where
    lu  = fmap (regexSource rty wc . macroSource) .
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
    mid_s = getMacroID mid
    neg_s = if is_neg then "-ve" else "+ve" :: String
    rty_s = presentRegexType rty
\end{code}
