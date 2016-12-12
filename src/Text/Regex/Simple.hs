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
  , substAllCaptures
  , substCaptures
  , substCaptures'
  , substCapture
  , captureMatches
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
--import qualified Data.IntMap                    as IntMap
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Text.Regex.Base

data SubMatches a =
  SubMatches
    { _sm_source :: !a
    , _sm_array  :: !(Array Int (Match a))
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

substAllCaptures :: (Monoid a,Extract a) => a -> SubMatches a -> a
substAllCaptures x = substCaptures' $ const $ const x

substCaptures :: (Monoid a,Extract a)
              => (Int->a->a)
              -> SubMatches a
              -> a
substCaptures f = substCaptures' f'
  where
    f' 0 = id
    f' i = f i

substCaptures' :: (Monoid a,Extract a)
               => (Int->a->a)
               -> SubMatches a
               -> a
substCaptures' f sm =
    foldr sc _sm_source $ zip [0..] $ left_to_right $ elems _sm_array
  where
    sc (i,m) hay   = substCapture (f i) (m { _m_hay = hay })

    SubMatches{..} = sm

substCapture :: (Monoid a,Extract a) => (a->a) -> Match a -> a
substCapture f m@Match{..} =
    matchPrefix m <> f _m_needle <> matchSuffix m

captureMatches :: [SubMatches a] -> SubMatches a
captureMatches []          = error "captureMatches: empty list"
captureMatches sms@(sm0:_) =
    mk $ left_to_right $ catMaybes $ map matchMay sms
  where
    mk as =
      SubMatches
        { _sm_source = _sm_source sm0
        , _sm_array  = listArray (0,length as-1) as
        }

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
subMatches' = elems . _sm_array

(%!) :: SubMatches a -> Int -> Match a
(%!) = subMatch

subMatch :: SubMatches a -> Int -> Match a
subMatch sm i = fromMaybe oops $ subMatch' sm i
  where
    oops = error $ "subMatch: out of bounds (" ++ show i ++ ")"

subMatch' :: SubMatches a -> Int -> Maybe (Match a)
subMatch' SubMatches{..} i = case bounds _sm_array `inRange` i of
  True  -> Just $ _sm_array ! i
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
cvt hay arr =
    SubMatches
      { _sm_source = hay
      , _sm_array  = fmap f arr
      }
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

-- NB matches and captures from the back end must be listed
-- left-to-right (i.e., ordered by _m_offset) for this to process
-- the captures bottom-up; we are not assuming the back ends are
-- meeting this requirement of regex-base because it is relativly easy
-- to ensure this critical property
left_to_right :: [Match a] -> [Match a]
left_to_right = sortBy $ comparing _m_offset
