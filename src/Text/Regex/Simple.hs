{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Text.Regex.Simple
  ( AllSubMatches(..)
  , SubMatches(..)
  , Match(..)
  , Subst(..)
  , Context(..)
  , Location(..)
  , substAllCaptures
  , substAllCaptures'
  , substCaptures
  , substCaptures'
  , substCapture
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

import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy.Char8     as LBS
import           Data.Array
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Text.Regex.Base


data AllSubMatches a =
  AllSubMatches
    { _asm_source :: !a
    , _asm_list   :: [SubMatches a]
    }
  deriving (Show)

data SubMatches a =
  SubMatches
    { _sm_source :: !a
    , _sm_array  :: !(Array Int (Match a))
    }
  deriving (Show)

data Match a =
  Match
    { _m_hay       :: !a
    , _m_needle    :: !a
    , _m_offset    :: !Int
    , _m_length    :: !Int
    }
  deriving (Show)

data Subst a =
  Subst
    { _subst_context :: Context
    , _subst_phi     :: Location -> a -> a
    }

data Context
  = TOP
  | CAP
  | ALL
  deriving (Show)

data Location =
  Location
    { _loc_match   :: Int
    , _loc_capture :: Int
    }
  deriving (Show)

substAllCaptures :: Subable a
                 => Subst a
                 -> AllSubMatches a
                 -> a
substAllCaptures subst = substAllCaptures' $ mk_phi subst

substAllCaptures' :: Subable a
                  => (Location->Match a->a)
                  -> AllSubMatches a
                  -> a
substAllCaptures' phi AllSubMatches{..} =
    substCaptures' f' $ SubMatches _asm_source arr
  where
    f' (Location _ i) = phi . uncurry Location $ arr_i ! i

    arr_i = listArray bds j_ks

    arr   = listArray bds $
        [ arr_ ! k
            | arr_ <- map _sm_array _asm_list
            , k <- indices arr_
            ]

    bds   = (0,length j_ks-1)

    j_ks  =
        [ (j,k)
            | (j,arr_) <- zip [0..] $ map _sm_array _asm_list
            , k <- indices arr_
            ]

substCaptures :: Subable a
              => Subst a
              -> SubMatches a
              -> a
substCaptures subst = substCaptures' $ mk_phi subst

substCaptures' :: Subable a
               => (Location->Match a->a)
               -> SubMatches a
               -> a
substCaptures' phi SubMatches{..} = fst $
    foldr sc (_sm_source,[]) $ zip [0..] $
      left_to_right $ elems _sm_array
  where
    sc (i,m0) (hay,ds) =
        ( substCapture (const ndl') m
        , (_m_offset m,len'-len) : ds
        )
      where
        len' = length_ ndl'
        len  = length_ ndl
        ndl' = phi (Location 0 i) m
        ndl  = _m_needle m
        m    = adj hay ds m0

    adj hay ds m =
      Match
        { _m_hay    = hay
        , _m_needle = before len $ after off0 hay
        , _m_offset = off0
        , _m_length = len
        }
      where
        len  = len0 + sum
          [ delta
            | (off,delta) <- ds
            , off < off0 + len0
            ]
        len0 = _m_length m
        off0 = _m_offset m

substCapture :: (Monoid a,Extract a) => (a->a) -> Match a -> a
substCapture f m@Match{..} =
    matchPrefix m <> f _m_needle <> matchSuffix m

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
  RegexContext regex source (AllSubMatches source) where
    match  r s = AllSubMatches s $ map (cvt s) $ match r s
    matchM r s = do
      y <- matchM r s
      return $ AllSubMatches s $ map (cvt s) y

class (Extract a,Monoid a) => Subable a where
  length_ :: a -> Int

instance (Extract [a],Monoid [a]) => Subable [a] where
  length_ = length

instance Subable B.ByteString where
  length_ = B.length

instance Subable LBS.ByteString where
  length_ = fromEnum . LBS.length

{-
instance Subable T.Text where
  length_ = T.length

instance Subable LT.Text where
  length_ = fromEnum . LT.length
-}

mk_phi :: Subst a -> Location -> Match a -> a
mk_phi Subst{..} loc0 = phi loc0 . _m_needle
  where
    phi = case _subst_context of
      TOP -> phi_top
      CAP -> phi_cap
      ALL -> _subst_phi

    phi_top loc@(Location _ n) = case n==0 of
      True  -> _subst_phi loc
      False -> id

    phi_cap loc@(Location _ n) = case n==0 of
      True  -> id
      False -> _subst_phi loc

cvt :: source -> MatchText source -> SubMatches source
cvt hay arr =
    SubMatches
      { _sm_source = hay
      , _sm_array  = fmap f arr
      }
  where
    f (ndl,(off,len)) =
      Match
        { _m_hay       = hay
        , _m_needle    = ndl
        , _m_offset    = off
        , _m_length    = len
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
