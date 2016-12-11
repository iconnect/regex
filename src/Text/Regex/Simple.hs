{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Text.Regex.Simple
  ( SubMatches(..)
  , Match(..)
  , matchNote
  , matched
  , matchMay
  , subMatches
  , subMatches'
  , (%!)
  , subMatch
  , subMatch'
  , matchText
  , matchOffset
  , matchLength
  , matchPrefix
  , matchSuffix
  ) where

import           Data.Array
import           Data.Maybe
import           Text.Regex.Base


newtype SubMatches a =
  SubMatches
    { _SubMatches :: Array Int (Match a)
    }
  deriving (Show)

data Match a =
  Match
    { _m_hay    :: !a
    , _m_needle :: !a
    , _m_offset :: !Int
    , _m_length :: !Int
    }
  deriving (Show)

matchNote :: String -> SubMatches a -> Match a
matchNote nte sm = fromMaybe oops $ matchMay sm
  where
    oops = error $ "matchNote: " ++ nte

matched :: SubMatches a -> Bool
matched = isJust . matchMay

matchMay :: SubMatches a -> Maybe (Match a)
matchMay sm = subMatch' sm 0

subMatches :: SubMatches a -> [Match a]
subMatches = tailDef [] . subMatches'

subMatches' :: SubMatches a -> [Match a]
subMatches' = elems . _SubMatches

(%!) :: SubMatches a -> Int -> Match a
(%!) = subMatch

subMatch :: SubMatches a -> Int -> Match a
subMatch sm i = fromMaybe oops $ subMatch' sm i
  where
    oops = error $ "subMatch: out of bounds (" ++ show i ++ ")"

subMatch' :: SubMatches a -> Int -> Maybe (Match a)
subMatch' SubMatches{..} i = case bounds _SubMatches `inRange` i of
  True  -> Just $ _SubMatches ! i
  False -> Nothing

matchText :: Match a -> a
matchText = _m_needle

matchOffset :: Match a -> Int
matchOffset = _m_offset

matchLength :: Match a -> Int
matchLength = _m_length

matchPrefix :: Extract a => Match a -> a
matchPrefix Match{..} = before _m_offset _m_hay

matchSuffix :: Extract a => Match a -> a
matchSuffix Match{..} = after (_m_offset+_m_length) _m_hay

instance RegexContext regex source (AllTextSubmatches (Array Int) (source,(Int,Int))) =>
  RegexContext regex source (SubMatches source) where
    match  r s = cvt s $ getAllTextSubmatches $ match r s
    matchM r s = do
      y <- matchM r s
      return $ cvt s $ getAllTextSubmatches y

instance RegexContext regex source [MatchText source] =>
  RegexContext regex source [SubMatches source] where
    match  r s = map (cvt s) $ match r s
    matchM r s = do
      y <- matchM r s
      return $ map (cvt s) y

cvt :: source -> MatchText source -> SubMatches source
cvt hay arr = SubMatches $ fmap f arr
  where
    f (ndl,(off,len)) =
      Match
        { _m_hay    = hay
        , _m_needle = ndl
        , _m_offset = off
        , _m_length = len
        }

tailDef :: [a] -> [a] -> [a]
tailDef df []    = df
tailDef _  (_:t) = t
