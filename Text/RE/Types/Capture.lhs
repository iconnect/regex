\begin{code}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveDataTypeable         #-}
\end{code}

\begin{code}
module Text.RE.Types.Capture
  ( Capture(..)
  , hasCaptured
  , capturePrefix
  , captureSuffix
  ) where
\end{code}

\begin{code}
import           Text.Regex.Base
\end{code}



\begin{code}
-- | the matching of a single sub-expression against part of the source
-- text
data Capture a =
  Capture
    { captureSource  :: !a    -- ^ the whole text that was searched
    , capturedText   :: !a    -- ^ the text that was matched
    , captureOffset  :: !Int  -- ^ the number of characters preceding the
                              -- match with -1 used if no text was captured
                              -- by the RE (not even the empty string)
    , captureLength  :: !Int  -- ^ the number of chacter in the captured
                              -- sub-string
    }
  deriving (Show,Eq)
\end{code}

\begin{code}
instance Functor Capture where
  fmap f c@Capture{..} =
    c
      { captureSource = f captureSource
      , capturedText = f capturedText
      }
\end{code}

\begin{code}
-- | test if the capture has matched any text
hasCaptured :: Capture a -> Bool
hasCaptured = (>=0) . captureOffset

-- | returns the text preceding the match
capturePrefix :: Extract a => Capture a -> a
capturePrefix Capture{..} = before captureOffset captureSource

-- | returns the text after the match
captureSuffix :: Extract a => Capture a -> a
captureSuffix Capture{..} = after (captureOffset+captureLength) captureSource
\end{code}
