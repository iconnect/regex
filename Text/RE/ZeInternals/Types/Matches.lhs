\begin{code}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveDataTypeable         #-}
\end{code}

\begin{code}
module Text.RE.ZeInternals.Types.Matches
  ( Matches(..)
  , anyMatches
  , countMatches
  , matches
  , mainCaptures
  ) where
\end{code}

\begin{code}
import           Data.Typeable
import           Text.RE.ZeInternals.Types.Capture
import           Text.RE.ZeInternals.Types.CaptureID
import           Text.RE.ZeInternals.Types.Match
import           Text.Regex.Base
\end{code}


\begin{code}
-- | the result of matching a RE against a text (with @*=~@), retaining
-- the text that was matched against
data Matches a =
  Matches
    { matchesSource :: !a          -- ^ the source text being matched
    , allMatches    :: ![Match a]  -- ^ all 'Match' instances found, left to right
    }
  deriving (Show,Eq,Typeable)
\end{code}

\begin{code}
instance Functor Matches where
  fmap f Matches{..} =
    Matches
      { matchesSource = f matchesSource
      , allMatches    = map (fmap f) allMatches
      }
\end{code}

\begin{code}
-- | tests whether the RE matched the source text at all
anyMatches :: Matches a -> Bool
anyMatches = not . null . allMatches

-- | count the matches
countMatches :: Matches a -> Int
countMatches = length . allMatches

-- | list the texts that Matched
matches :: Matches a -> [a]
matches = map capturedText . mainCaptures

-- | extract the main capture from each match
mainCaptures :: Matches a -> [Capture a]
mainCaptures ac = [ capture c0 cs | cs<-allMatches ac ]
  where
    c0 = IsCaptureOrdinal $ CaptureOrdinal 0
\end{code}

\begin{code}
-- | this instance hooks 'Matches' into regex-base: regex consumers need
-- not worry about any of this
instance
    ( RegexContext regex source [MatchText source]
    , RegexLike    regex source
    , RegexFix     regex source
    ) =>
  RegexContext regex source (Matches source) where
    match  r s = Matches s $ map (convertMatchText r s) $ match r s
    matchM r s = do
      y <- matchM r s
      return $ Matches s $ map (convertMatchText r s) y
\end{code}
