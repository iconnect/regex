{-# LANGUAGE RecordWildCards            #-}

module Main(main) where

import       Data.Array
import       Text.Regex.Simple
import       Text.Regex.TDFA


main :: IO ()
main = do
  ex
  exM
  ex_all
  ex_allM
  ex_subst
  ex_all_subst
  ex_allMFail
  print ex_sm
  print all_sm
  print m1
  print m2

str, str', regex :: String
str   = "a bbbb aa b"
str'  = "foo"
regex = "(a+) (b+)"

ex :: IO ()
ex = do
  let sm = str =~ regex :: SubMatches String
      m  = matchNote "myExample" sm
  print sm
  print $ _m_hay      m
  print $ matchText   m
  print $ matchPrefix m
  print $ matchSuffix m

exM :: IO ()
exM = do
  matches <- str =~~ regex :: IO (SubMatches String)
  print matches

ex_all :: IO ()
ex_all = do
  let matches = str =~ regex :: (AllSubMatches String)
  print matches

ex_allM :: IO ()
ex_allM = do
  matches <- str =~~ regex :: IO (AllSubMatches String)
  print matches

ex_allMFail :: IO ()
ex_allMFail = do
  ms <- str' =~~ regex :: IO (AllSubMatches String)
  print ms

ex_subst :: IO ()
ex_subst = do
    ms <- str =~~ regex :: IO (SubMatches String)
    print $ substCaptures' f ms
  where
    f i Match{..} = "(" ++ show i ++ ":" ++ _m_needle ++ ")"

ex_all_subst :: IO ()
ex_all_subst = do
    ms <- str =~~ regex :: IO (AllSubMatches String)
    print $ substAllCaptures' f ms
  where
    f (Location i j) Match{..} = "(" ++ show i ++ ":" ++ show j ++ ":" ++ _m_needle ++ ")"

ex_sm :: SubMatches String
ex_sm =
    SubMatches
      { _sm_source = "a bbbb aa b"
      , _sm_array = array (0,2)
          [ (0, Match {_m_hay = "a bbbb aa b", _m_needle = "a bbbb", _m_offset = 0,  _m_length = 6})
          , (1, Match {_m_hay = "a bbbb aa b", _m_needle = "a"     , _m_offset = 0,  _m_length = 1})
          , (2, Match {_m_hay = "a bbbb aa b", _m_needle = "bbbb"  , _m_offset = 2,  _m_length = 4})
          ]
      }

all_sm :: [SubMatches String]
all_sm =
  [ SubMatches
      { _sm_source = "a bbbb aa b"
      , _sm_array = array (0,2)
          [ (0, Match {_m_hay = "a bbbb aa b", _m_needle = "a bbbb", _m_offset = 0,  _m_length = 6})
          , (1, Match {_m_hay = "a bbbb aa b", _m_needle = "a"     , _m_offset = 0,  _m_length = 1})
          , (2, Match {_m_hay = "a bbbb aa b", _m_needle = "bbbb"  , _m_offset = 2,  _m_length = 4})
          ]
      }
  , SubMatches
      { _sm_source = "a bbbb aa b"
      , _sm_array = array (0,2)
          [ (0, Match {_m_hay = "a bbbb aa b", _m_needle = "aa b"  , _m_offset = 7,  _m_length = 4})
          , (1, Match {_m_hay = "a bbbb aa b", _m_needle = "aa"    , _m_offset = 7,  _m_length = 2})
          , (2, Match {_m_hay = "a bbbb aa b", _m_needle = "b"     , _m_offset = 10, _m_length = 1})
          ]
      }
  ]

m1 :: Match String
m1 = Match {_m_hay = "a bbbb aa b", _m_needle = "a"     , _m_offset = 0,  _m_length = 1}

m2 :: Match String
m2 = Match {_m_hay = "a bbbb aa b", _m_needle = "bbbb"  , _m_offset = 2,  _m_length = 4}
