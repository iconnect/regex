{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Text.RE.TDFA.Text.Lazy
  ( (*=~)
  , (?=~)
  , (=~)
  , (=~~)
  , module Text.RE
  , module Text.RE.TDFA.RE
  ) where

import qualified Data.Text.Lazy                as TL
import           Text.Regex.Base
import           Text.RE
import           Text.RE.Internal.AddCaptureNames
import           Text.RE.TDFA.RE
import qualified Text.Regex.TDFA               as TDFA


(*=~) :: TL.Text
      -> RE
      -> Matches TL.Text
(*=~) bs rex = addCaptureNamesToMatches (reCaptureNames rex) $ match (reRegex rex) bs

(?=~) :: TL.Text
      -> RE
      -> Match TL.Text
(?=~) bs rex = addCaptureNamesToMatch (reCaptureNames rex) $ match (reRegex rex) bs

(=~) :: ( RegexContext TDFA.Regex TL.Text a
        , RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption String
        )
     => TL.Text
     -> RE
     -> a
(=~) bs rex = match (reRegex rex) bs

(=~~) :: ( Monad m
         , RegexContext TDFA.Regex TL.Text a
         , RegexMaker   TDFA.Regex TDFA.CompOption TDFA.ExecOption String
         )
      => TL.Text
      -> RE
      -> m a
(=~~) bs rex = matchM (reRegex rex) bs

instance IsRegex RE TL.Text where
  matchOnce   = flip (?=~)
  matchMany   = flip (*=~)
  regexSource = reSource
