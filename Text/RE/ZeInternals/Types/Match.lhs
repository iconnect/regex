\begin{code}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE MonoLocalBinds             #-}
\end{code}

\begin{code}
module Text.RE.ZeInternals.Types.Match
  ( Match(..)
  , noMatch
  , emptyMatchArray
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
  , RegexFix(..)
  , convertMatchText
  ) where
\end{code}

\begin{code}
import           Data.Array
import           Data.Bits
import qualified Data.ByteString                as BW
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy.Char8     as LBS
import qualified Data.ByteString.UTF8           as B
import           Data.Maybe
import qualified Data.Sequence                  as S
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.Lazy                 as LT
import           Data.Typeable
import           Data.Word
import           Text.RE.ZeInternals.Types.Capture
import           Text.RE.ZeInternals.Types.CaptureID
import           Text.Regex.Base
import qualified Text.Regex.PCRE                as PCRE
import qualified Text.Regex.TDFA                as TDFA

infixl 9 !$, !$$
\end{code}

\begin{code}
-- | the result of matching a RE to a text once (with @?=~@), retaining
-- the text that was matched against
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
-- | Construct a Match that does not match anything.
noMatch :: a -> Match a
noMatch t = Match t noCaptureNames emptyMatchArray

-- | an empty array of Capture
emptyMatchArray :: Array CaptureOrdinal (Capture a)
emptyMatchArray = listArray (CaptureOrdinal 0,CaptureOrdinal $ -1) []
\end{code}

\begin{code}
instance Functor Match where
  fmap f Match{..} =
    Match
      { matchSource  = f matchSource
      , captureNames = captureNames
      , matchArray   = fmap (fmap f) matchArray
      }
\end{code}

\begin{code}
-- | tests whether the RE matched the source text at all
matched :: Match a -> Bool
matched = isJust . matchCapture

-- | yields the text matched by the RE, Nothing if no match
matchedText :: Match a -> Maybe a
matchedText = fmap capturedText . matchCapture

-- | the top-level capture if the source text matched the RE,
-- Nothing otherwise
matchCapture :: Match a -> Maybe (Capture a)
matchCapture = fmap fst . matchCaptures

-- | the main top-level capture (capture \'0'') and the sub captures
-- if the text matched the RE, @Nothing@ otherwise
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
  i   <- lookupCaptureID cid mtch
  cap <- case bounds matchArray `inRange` i of
    True  -> Just $ matchArray ! i
    False -> Nothing
  case hasCaptured cap of
    True  -> Just cap
    False -> Nothing

lookupCaptureID :: CaptureID -> Match a -> Maybe CaptureOrdinal
lookupCaptureID cid Match{..} =
    either (const Nothing) Just $ findCaptureID cid captureNames
\end{code}


\begin{code}
-- | this instance hooks 'Match' into regex-base: regex consumers need
-- not worry about any of this
instance
    ( RegexContext regex source (AllTextSubmatches (Array Int) (source,(Int,Int)))
    , RegexLike    regex source
    , RegexFix     regex source
    ) =>
  RegexContext regex source (Match source) where
    match  r s = convertMatchText r s $ getAllTextSubmatches $ match r s
    matchM r s = do
      y <- matchM r s
      return $ convertMatchText r s $ getAllTextSubmatches y
\end{code}

\begin{code}
-- | convert a regex-base native MatchText into a regex Match type
convertMatchText :: RegexFix regex source
                 => regex
                 -> source
                 -> MatchText source
                 -> Match source
convertMatchText re hay arr =
    Match
      { matchSource  = hay
      , captureNames = noCaptureNames
      , matchArray   =
          ixmap (CaptureOrdinal lo,CaptureOrdinal hi) getCaptureOrdinal $
            fmap f arr
      }
  where
    (lo,hi) = bounds arr

    f (ndl,(off_,len_)) =
      Capture
        { captureSource = hay
        , capturedText  = ndl
        , captureOffset = off
        , captureLength = len
        }
      where
        CharRange off len = utf8_correct re hay off_ len_
\end{code}

\begin{code}
data CharRange = CharRange !Int !Int
  deriving (Show)

class RegexFix regex source where
  utf8_correct :: regex -> source -> Int -> Int -> CharRange
  utf8_correct _ _ = CharRange

instance RegexFix TDFA.Regex [Char]         where
instance RegexFix TDFA.Regex B.ByteString   where
instance RegexFix TDFA.Regex LBS.ByteString where
instance RegexFix TDFA.Regex T.Text         where
instance RegexFix TDFA.Regex LT.Text        where
instance RegexFix TDFA.Regex (S.Seq Char)   where

instance RegexFix PCRE.Regex [Char]         where
  utf8_correct _ = utf8_correct_bs . B.fromString
instance RegexFix PCRE.Regex B.ByteString   where
instance RegexFix PCRE.Regex LBS.ByteString where
instance RegexFix PCRE.Regex T.Text         where
  utf8_correct _ = utf8_correct_bs . T.encodeUtf8
instance RegexFix PCRE.Regex LT.Text        where
  utf8_correct _ = utf8_correct_bs . T.encodeUtf8 . LT.toStrict
instance RegexFix PCRE.Regex (S.Seq Char)   where

-- convert a byte offset+length in a UTF-8-encoded ByteString
-- into a character offset+length
utf8_correct_bs :: B.ByteString -> Int -> Int -> CharRange
utf8_correct_bs bs ix0 ln0 = case ix0+ln0 > BW.length bs of
    True  -> error "utf8_correct_bs: index+length out of range"
    False -> skip 0 0     -- BW.index calls below should not fail
  where
    skip ix di = case compare ix ix0 of
      GT -> error "utf8_correct_bs: UTF-8 decoding error"
      EQ -> count ix di 0 ln0
      LT -> case u8_width $ BW.index bs ix of
        Single    -> skip (ix+1)   di
        Double    -> skip (ix+2) $ di+1
        Triple    -> skip (ix+3) $ di+2
        Quadruple -> skip (ix+4) $ di+3

    count ix di dl c = case compare c 0 of
      LT -> error "utf8_correct_bs: length ends inside character"
      EQ -> CharRange (ix0-di) (ln0-dl)
      GT -> case u8_width $ BW.index bs ix of
        Single    -> count (ix+1) di  dl    $ c-1
        Double    -> count (ix+2) di (dl+1) $ c-2
        Triple    -> count (ix+3) di (dl+2) $ c-3
        Quadruple -> count (ix+4) di (dl+3) $ c-4

data UTF8Size = Single | Double | Triple | Quadruple
  deriving (Show)

u8_width :: Word8 -> UTF8Size
u8_width w8 = case   w8 .&. 0x80 == 0x00 of
  True  ->       Single
  False -> case      w8 .&. 0xE0 == 0xC0 of
    True  ->     Double
    False -> case    w8 .&. 0xF0 == 0xE0 of
      True  ->   Triple
      False -> case  w8 .&. 0xF8 == 0xF0 of
        True  -> Quadruple
        False -> error "u8_width: UTF-8 decoding error"
\end{code}
