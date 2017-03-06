\begin{code}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveDataTypeable         #-}
\end{code}

\begin{code}
module Text.RE.Capture
  ( Matches(..)
  , Match(..)
  , Capture(..)
  , noMatch
  , emptyMatchArray
  -- Matches functions
  , anyMatches
  , countMatches
  , matches
  , mainCaptures
  -- Match functions
  , matched
  , matchedText
  , matchCapture
  , matchCaptures
  , (!$$)
  , captureText
  , (!$$?)
  , captureTextMaybe
  , (!$)
  , capture
  , (!$?)
  , captureMaybe
  -- Capture functions
  , hasCaptured
  , capturePrefix
  , captureSuffix
  ) where
\end{code}

\begin{code}
import           Data.Array
import           Data.Maybe
import           Data.Typeable
import           Text.Regex.Base
import           Text.RE.CaptureID

infixl 9 !$, !$$
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
-- | the result of matching a RE to a text once, listing the text that
-- was matched and the named captures in the RE and all of the substrings
-- matched, with the text captured by the whole RE; a complete failure
-- to match will be represented with an empty array (with bounds (0,-1))
data Match a =
  Match
    { matchSource  :: !a                -- ^ the whole source text
    , captureNames :: !CaptureNames     -- ^ the RE's capture names
    , matchArray   :: !(Array CaptureOrdinal (Capture a))
                                        -- ^ 0..n-1 captures,
                                        -- starting with the
                                        -- text matched by the
                                        -- whole RE
    }
  deriving (Show,Eq,Typeable)
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
-- | Construct a Match that does not match anything.
noMatch :: a -> Match a
noMatch t = Match t noCaptureNames emptyMatchArray

-- | an empty array of Capture
emptyMatchArray :: Array CaptureOrdinal (Capture a)
emptyMatchArray = listArray (CaptureOrdinal 0,CaptureOrdinal $ -1) []
\end{code}

\begin{code}
instance Functor Matches where
  fmap f Matches{..} =
    Matches
      { matchesSource = f matchesSource
      , allMatches    = map (fmap f) allMatches
      }

instance Functor Match where
  fmap f Match{..} =
    Match
      { matchSource  = f matchSource
      , captureNames = captureNames
      , matchArray   = fmap (fmap f) matchArray
      }

instance Functor Capture where
  fmap f c@Capture{..} =
    c
      { captureSource = f captureSource
      , capturedText = f capturedText
      }
\end{code}

\begin{code}
-- | tests whether the RE matched the source text at all
anyMatches :: Matches a -> Bool
anyMatches = not . null . allMatches

-- | count the matches
countMatches :: Matches a -> Int
countMatches = length . allMatches

matches :: Matches a -> [a]
matches = map capturedText . mainCaptures

-- | extract the main capture from each match
mainCaptures :: Matches a -> [Capture a]
mainCaptures ac = [ capture c0 cs | cs<-allMatches ac ]
  where
    c0 = IsCaptureOrdinal $ CaptureOrdinal 0
\end{code}



\begin{code}
-- | tests whether the RE matched the source text at all
matched :: Match a -> Bool
matched = isJust . matchCapture

-- | tests whether the RE matched the source text at all
matchedText :: Match a -> Maybe a
matchedText = fmap capturedText . matchCapture

-- | the top-level capture if the source text matched the RE,
-- Nothing otherwise
matchCapture :: Match a -> Maybe (Capture a)
matchCapture = fmap fst . matchCaptures

-- | the top-level capture and the sub captures if the text matched
-- the RE, Nothing otherwise
matchCaptures :: Match a -> Maybe (Capture a,[Capture a])
matchCaptures Match{..} = case rangeSize (bounds matchArray) == 0 of
  True  -> Nothing
  False -> Just (matchArray!0,drop 1 $ elems matchArray)

-- | an alternative for captureText
(!$$) :: Match a -> CaptureID -> a
(!$$) = flip captureText

-- | look up the text of the nth capture, 0 being the match of the whole
-- RE against the source text, 1, the first bracketed sub-expression to
-- be matched and so on
captureText :: CaptureID -> Match a -> a
captureText cid mtch = capturedText $ capture cid mtch

-- | an alternative for captureTextMaybe
(!$$?) :: Match a -> CaptureID -> Maybe a
(!$$?) = flip captureTextMaybe

-- | look up the text of the nth capture (0 being the match of the
-- whole), returning Nothing if the Match doesn't contain the capture
captureTextMaybe :: CaptureID -> Match a -> Maybe a
captureTextMaybe cid mtch = do
    cap <- mtch !$? cid
    case hasCaptured cap of
      True  -> Just $ capturedText cap
      False -> Nothing

-- | an alternative for capture
(!$) :: Match a -> CaptureID -> Capture a
(!$) = flip capture

-- | look up the nth capture, 0 being the match of the whole RE against
-- the source text, 1, the first bracketed sub-expression to be matched
-- and so on
capture :: CaptureID -> Match a -> Capture a
capture cid mtch = fromMaybe oops $ mtch !$? cid
  where
    oops = error $ "capture: out of bounds (" ++ show cid ++ ")"

-- | an alternative for capture captureMaybe
(!$?) :: Match a -> CaptureID -> Maybe (Capture a)
(!$?) = flip captureMaybe

-- | look up the nth capture, 0 being the match of the whole RE against
-- the source text, 1, the first bracketed sub-expression to be matched
-- and so on, returning Nothing if there is no such capture, or if the
-- capture failed to capture anything (being in a failed alternate)
captureMaybe :: CaptureID -> Match a -> Maybe (Capture a)
captureMaybe cid mtch@Match{..} = do
  cap <- case bounds matchArray `inRange` CaptureOrdinal i of
    True  -> Just $ matchArray ! CaptureOrdinal i
    False -> Nothing
  case hasCaptured cap of
    True  -> Just cap
    False -> Nothing
  where
    i = lookupCaptureID cid mtch

lookupCaptureID :: CaptureID -> Match a -> Int
lookupCaptureID cid Match{..} = findCaptureID cid captureNames
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


\begin{code}
-- | for matching just the first RE against the source text
instance
    ( RegexContext regex source (AllTextSubmatches (Array Int) (source,(Int,Int)))
    , RegexLike    regex source
    ) =>
  RegexContext regex source (Match source) where
    match  r s = cvt s $ getAllTextSubmatches $ match r s
    matchM r s = do
      y <- matchM r s
      return $ cvt s $ getAllTextSubmatches y

-- | for matching all REs against the source text
instance
    ( RegexContext regex source [MatchText source]
    , RegexLike    regex source
    ) =>
  RegexContext regex source (Matches source) where
    match  r s = Matches s $ map (cvt s) $ match r s
    matchM r s = do
      y <- matchM r s
      return $ Matches s $ map (cvt s) y
\end{code}

\begin{code}
cvt :: source -> MatchText source -> Match source
cvt hay arr =
    Match
      { matchSource  = hay
      , captureNames = noCaptureNames
      , matchArray   =
          ixmap (CaptureOrdinal lo,CaptureOrdinal hi) getCaptureOrdinal $
            fmap f arr
      }
  where
    (lo,hi) = bounds arr

    f (ndl,(off,len)) =
      Capture
        { captureSource = hay
        , capturedText  = ndl
        , captureOffset = off
        , captureLength = len
        }
\end{code}
