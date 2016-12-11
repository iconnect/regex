module Main(main) where

import       Text.Regex.Simple
import       Text.Regex.TDFA


main :: IO ()
main = do
  myExample
  myExampleM
  myExampleAll
  myExampleAllM
  myExampleAllMFail


str, str', regex :: String
str   = "a bbbb aa b"
str'  = "foo"
regex = "(a+) (b+)"

myExample :: IO ()
myExample = do
  let sm = str =~ regex :: SubMatches String
      m  = matchNote "myExample" sm
  print sm
  print $ _m_hay      m
  print $ matchText   m
  print $ matchPrefix m
  print $ matchSuffix m

myExampleM :: IO ()
myExampleM = do
  matches <- str =~~ regex :: IO (SubMatches String)
  print matches

myExampleAll :: IO ()
myExampleAll = do
  let matches = str =~ regex :: [SubMatches String]
  print matches

myExampleAllM :: IO ()
myExampleAllM = do
  matches <- str =~~ regex :: IO [SubMatches String]
  print matches

myExampleAllMFail :: IO ()
myExampleAllMFail = do
  matches <- str' =~~ regex :: IO [SubMatches String]
  print matches
