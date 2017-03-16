\begin{code}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveDataTypeable         #-}
\end{code}

\begin{code}
module Text.RE.Types.Matches
  ( Matches(..)
  , anyMatches
  , countMatches
  , matches
  , mainCaptures
  ) where
\end{code}

\begin{code}
import           Data.Typeable
import           Text.Regex.Base
import           Text.RE.Types.Capture
import           Text.RE.Types.CaptureID
import           Text.RE.Types.Match
\end{code}



\begin{code}
-- | the result type to use when every match is needed, not just the
-- first match of the RE against the source
data Matches a =
  Matches
    { matchesSource :: !a          -- ^ the source text being matched
    , allMatches    :: ![Match a]  -- ^ all captures found, left to right
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

-- | list the Matches
matches :: Matches a -> [a]
matches = map capturedText . mainCaptures

-- | extract the main capture from each match
mainCaptures :: Matches a -> [Capture a]
mainCaptures ac = [ capture c0 cs | cs<-allMatches ac ]
  where
    c0 = IsCaptureOrdinal $ CaptureOrdinal 0
\end{code}


\begin{code}
-- | for matching all REs against the source text
instance
    ( RegexContext regex source [MatchText source]
    , RegexLike    regex source
    ) =>
  RegexContext regex source (Matches source) where
    match  r s = Matches s $ map (convertMatchText s) $ match r s
    matchM r s = do
      y <- matchM r s
      return $ Matches s $ map (convertMatchText s) y
\end{code}
