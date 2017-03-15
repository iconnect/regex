\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}

module Text.RE.Types.Replace
  ( Replace(..)
  , ReplaceMethods(..)
  , replaceMethods
  , Context(..)
  , Location(..)
  , isTopLocation
  , replace
  , replaceAll
  , replaceAllCaptures
  , replaceAllCaptures_
  , replaceAllCapturesM
  , replaceCaptures
  , replaceCaptures_
  , replaceCapturesM
  , expandMacros
  , expandMacros'
  ) where

import           Control.Applicative
import           Data.Array
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy.Char8     as LBS
import           Data.Char
import qualified Data.Foldable                  as F
import           Data.Functor.Identity
import qualified Data.HashMap.Strict            as HM
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence                  as S
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Text.Lazy                 as LT
import           Prelude.Compat
import           Text.Heredoc
import           Text.RE.Types.Capture
import           Text.RE.Types.CaptureID
import           Text.RE.Types.Match
import           Text.RE.Types.Matches
import           Text.RE.Types.Options
import           Text.Read
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Text()
import           Text.Regex.TDFA.Text.Lazy()
\end{code}

\begin{code}
-- | Replace provides the missing methods needed to replace the matched
-- text; lengthE is the minimum implementation
class (Extract a,Monoid a) => Replace a where
  -- | length function for a
  lengthE        :: a -> Int
  -- | inject String into a
  packE          :: String -> a
  -- | project a onto a String
  unpackE        :: a -> String
  -- | inject into Text
  textifyE       :: a -> T.Text
  -- | project Text onto a
  detextifyE     :: T.Text -> a
  -- | split into lines
  linesE         :: a -> [a]
  -- | concatenate a list of lines
  unlinesE       :: [a] -> a
  -- | append a newline
  appendNewlineE :: a -> a
  -- | apply a substitution function to a Capture
  substE         :: (a->a) -> Capture a -> a
  -- | convert a template containing $0, $1, etc., in the first
  -- argument, into a 'phi' replacement function for use with
  -- replaceAllCaptures and replaceCaptures
  parseTemplateE :: a -> Match a -> Location -> Capture a -> Maybe a

  textifyE       = T.pack . unpackE
  detextifyE     = packE  . T.unpack
  appendNewlineE = (<> packE "\n")

  substE f m@Capture{..} =
    capturePrefix m <> f capturedText <> captureSuffix m
\end{code}

\begin{code}
-- | a selction of the Replace methods can be encapsulated with ReplaceMethods
-- for the higher-order replacement functions
data ReplaceMethods a =
  ReplaceMethods
    { methodLength :: a -> Int
    , methodSubst  :: (a->a) -> Capture a -> a
    }

-- | replaceMethods encapsulates ReplaceMethods a from a Replace a context
replaceMethods :: Replace a => ReplaceMethods a
replaceMethods =
  ReplaceMethods
    { methodLength = lengthE
    , methodSubst  = substE
    }
\end{code}

\begin{code}
-- | @Context@ specifies which contexts the substitutions should be applied
data Context
  = TOP   -- ^ substitutions should be applied to the top-level only,
          -- the text that matched the whole RE
  | SUB   -- ^ substitutions should only be applied to the text
          -- captured by bracketed sub-REs
  | ALL   -- ^ the substitution function should be applied to all
          -- captures, the top level and the sub-expression captures
  deriving (Show)

-- | the @Location@ information passed into the substitution function
-- specifies which sub-expression is being substituted
data Location =
  Location
    { locationMatch   :: Int
                        -- ^ the zero-based, i-th string to be matched,
                        -- when matching all strings, zero when only the
                        -- first string is being matched
    , locationCapture :: CaptureOrdinal
                        -- ^ 0, when matching the top-level string
                        -- matched by the whole RE, 1 for the top-most,
                        -- left-most redex captured by bracketed
                        -- sub-REs, etc.
    }
  deriving (Show)
\end{code}

\begin{code}
-- | True iff the location references a complete match
-- (i.e., not a bracketed capture)
isTopLocation :: Location -> Bool
isTopLocation = (==0) . locationCapture
\end{code}

\begin{code}
-- | replace all with a template, $0 for whole text, $1 for first
-- capture, etc.
replaceAll :: Replace a
           => a
           -> Matches a
           -> a
replaceAll tpl ac = replaceAllCaptures TOP (parseTemplateE tpl) ac
\end{code}

\begin{code}
-- | substitutes using a function that takes the full Match
-- context and returns the same replacement text as the _phi_phi
-- context.
replaceAllCaptures :: Replace a
                   => Context
                   -> (Match a->Location->Capture a->Maybe a)
                   -> Matches a
                   -> a
\end{code}

\begin{code}
replaceAllCaptures = replaceAllCaptures_ replaceMethods
\end{code}

\begin{code}
-- | replaceAllCaptures_ is like like replaceAllCaptures but takes the
-- Replace methods through the ReplaceMethods argument
replaceAllCaptures_ :: Extract a
                    => ReplaceMethods a
                    -> Context
                    -> (Match a->Location->Capture a->Maybe a)
                    -> Matches a
                    -> a
replaceAllCaptures_ s ctx phi ac =
    runIdentity $ replaceAllCapturesM s ctx (lift_phi phi) ac
\end{code}

\begin{code}
-- | replaceAllCapturesM is just a monadically generalised version of
-- replaceAllCaptures_
replaceAllCapturesM :: (Extract a,Monad m)
                    => ReplaceMethods a
                    -> Context
                    -> (Match a->Location->Capture a->m (Maybe a))
                    -> Matches a
                    -> m a
replaceAllCapturesM r ctx phi_ Matches{..} =
    replaceCapturesM r ALL phi $ Match matchesSource cnms arr
  where
    phi _ (Location _ i) = case arr_c!i of
      Just caps -> phi_ caps . uncurry Location $ arr_i ! i
      Nothing   -> const $ return Nothing

    arr_c = listArray bds $
      concat $
        [ repl (rangeSize $ bounds $ matchArray cs) cs
            | cs <- allMatches
            ]

    arr_i = listArray bds j_ks

    arr   = listArray bds $
        [ arr_ ! k
            | arr_ <- map matchArray allMatches
            , k    <- indices arr_
            ]

    bds   = (0,CaptureOrdinal $ length j_ks-1)

    j_ks  =
        [ (j,k)
            | (j,arr_) <- zip [0..] $ map matchArray allMatches
            ,  k       <- indices arr_
            ]

    repl 0 _ = []
    repl n x = case ctx of
      TOP -> Just x  : replicate (n-1) Nothing
      SUB -> Nothing : replicate (n-1) (Just x)
      ALL -> replicate n $ Just x

    cnms = fromMaybe noCaptureNames $ listToMaybe $ map captureNames allMatches
\end{code}

\begin{code}
-- | replace with a template containing $0 for whole text,
-- $1 for first capture, etc.
replace :: Replace a
        => Match a
        -> a
        -> a
replace c tpl = replaceCaptures TOP (parseTemplateE tpl) c
\end{code}

\begin{code}
-- | substitutes using a function that takes the full Match
-- context and returns the same replacement text as the _phi_phi
-- context.
replaceCaptures :: Replace a
                 => Context
                 -> (Match a->Location->Capture a->Maybe a)
                 -> Match a
                 -> a
replaceCaptures = replaceCaptures_ replaceMethods
\end{code}

\begin{code}
-- | replaceCaptures_ is like replaceCaptures but takes the Replace methods
-- through the ReplaceMethods argument
replaceCaptures_ :: Extract a
                 => ReplaceMethods a
                 -> Context
                 -> (Match a->Location->Capture a->Maybe a)
                 -> Match a
                 -> a
replaceCaptures_ s ctx phi caps =
  runIdentity $ replaceCapturesM s ctx (lift_phi phi) caps
\end{code}

\begin{code}
-- | replaceCapturesM is just a monadically generalised version of
-- replaceCaptures_
replaceCapturesM :: (Monad m,Extract a)
                 => ReplaceMethods a
                 -> Context
                 -> (Match a->Location->Capture a->m (Maybe a))
                 -> Match a
                 -> m a
replaceCapturesM ReplaceMethods{..} ctx phi_ caps@Match{..} = do
    (hay',_) <- foldr sc (return (matchSource,[])) $
                    zip [0..] $ elems matchArray
    return hay'
  where
    sc (i,cap0) act = do
      (hay,ds) <- act
      let ndl  = capturedText cap
          cap  = adj hay ds cap0
      mb <- phi i cap
      case mb of
        Nothing   -> return (hay,ds)
        Just ndl' ->
            return
              ( methodSubst (const ndl') cap
              , (captureOffset cap,len'-len) : ds
              )
          where
            len' = methodLength ndl'
            len  = methodLength ndl

    adj hay ds cap =
      Capture
        { captureSource = hay
        , capturedText  = before len $ after off0 hay
        , captureOffset = off0
        , captureLength = len
        }
      where
        len  = len0 + sum
          [ delta
            | (off,delta) <- ds
            , off < off0 + len0
            ]
        len0 = captureLength cap
        off0 = captureOffset cap

    phi i cap = case ctx of
      TOP | i/=0 -> return Nothing
      SUB | i==0 ->return  Nothing
      _          ->
        case not $ hasCaptured cap of
          True  -> return Nothing
          False -> phi_ caps (Location 0 i) cap
\end{code}

\begin{code}
-- the Replace instances

instance Replace [Char] where
  lengthE         = length
  packE           = id
  unpackE         = id
  textifyE        = T.pack
  detextifyE      = T.unpack
  linesE          = lines
  unlinesE        = unlines
  appendNewlineE  = (<>"\n")
  parseTemplateE  = parseTemplateE' id

instance Replace B.ByteString where
  lengthE         = B.length
  packE           = B.pack
  unpackE         = B.unpack
  textifyE        = TE.decodeUtf8
  detextifyE      = TE.encodeUtf8
  linesE          = B.lines
  unlinesE        = B.unlines
  appendNewlineE  = (<>"\n")
  parseTemplateE  = parseTemplateE' B.unpack

instance Replace LBS.ByteString where
  lengthE         = fromEnum . LBS.length
  packE           = LBS.pack
  unpackE         = LBS.unpack
  textifyE        = TE.decodeUtf8  . LBS.toStrict
  linesE          = LBS.lines
  unlinesE        = LBS.unlines
  detextifyE      = LBS.fromStrict . TE.encodeUtf8
  appendNewlineE  = (<>"\n")
  parseTemplateE  = parseTemplateE' LBS.unpack

instance Replace (S.Seq Char) where
  lengthE         = S.length
  packE           = S.fromList
  unpackE         = F.toList
  linesE          = map packE . lines . unpackE
  unlinesE        = packE . unlines . map unpackE
  parseTemplateE  = parseTemplateE' F.toList

instance Replace T.Text where
  lengthE         = T.length
  packE           = T.pack
  unpackE         = T.unpack
  textifyE        = id
  detextifyE      = id
  linesE          = T.lines
  unlinesE        = T.unlines
  appendNewlineE  = (<>"\n")
  parseTemplateE  = parseTemplateE' T.unpack

instance Replace LT.Text where
  lengthE         = fromEnum . LT.length
  packE           = LT.pack
  unpackE         = LT.unpack
  textifyE        = LT.toStrict
  detextifyE      = LT.fromStrict
  linesE          = LT.lines
  unlinesE        = LT.unlines
  appendNewlineE  = (<>"\n")
  parseTemplateE  = parseTemplateE' LT.unpack
\end{code}

\begin{code}
-- | expand all of the @{..} macros in the RE in the argument String
-- according to the Macros argument, preprocessing the RE String
-- according to the Mode argument (used internally)
expandMacros :: (r->String) -> Macros r -> String -> String
expandMacros x_src hm s =
  case HM.null hm of
    True  -> s
    False -> expandMacros' (fmap x_src . flip HM.lookup hm) s
\end{code}

\begin{code}
-- | expand the @{..} macos in the argument string using the given
-- function
expandMacros' :: (MacroID->Maybe String) -> String -> String
expandMacros' lu = fixpoint e_m
  where
    e_m re_s = replaceAllCaptures TOP phi $ re_s $=~ [here|@(@|\{([^{}]+)\})|]
      where
        phi mtch _ cap = case txt == "@@" of
            True  -> Just   "@"
            False -> Just $ fromMaybe txt $ lu ide
          where
            txt = capturedText cap
            ide = MacroID $ capturedText $ capture c2 mtch
            c2  = IsCaptureOrdinal $ CaptureOrdinal 2
\end{code}

\begin{code}
lift_phi :: Monad m
         => (Match a->Location->Capture a->Maybe a)
         -> (Match a->Location->Capture a->m (Maybe a))
lift_phi phi_ = phi
  where
    phi caps' loc' cap' = return $ phi_ caps' loc' cap'
\end{code}

\begin{code}
parseTemplateE' :: ( Replace a
                   , RegexContext Regex a (Matches a)
                   , RegexMaker   Regex CompOption ExecOption String
                   )
                   => (a->String)
                   -> a
                   -> Match a
                   -> Location
                   -> Capture a
                   -> Maybe a
parseTemplateE' unpack tpl mtch _ _ =
    Just $ replaceAllCaptures TOP phi $
      tpl $=~ [here|\$(\$|[0-9]|\{([^{}]+)\})|]
  where
    phi t_mtch _ _ = case t_mtch !$? c2 of
      Just cap -> case readMaybe stg of
          Nothing -> this $ IsCaptureName    $ CaptureName $ T.pack stg
          Just cn -> this $ IsCaptureOrdinal $ CaptureOrdinal cn
        where
          stg = unpack $ capturedText cap
      Nothing -> case s == "$" of
        True  -> Just t
        False -> this $ IsCaptureOrdinal $ CaptureOrdinal $ read s
      where
        s = unpack t
        t = capturedText $ capture c1 t_mtch

        this cid = capturedText <$> mtch !$? cid

    c1 = IsCaptureOrdinal $ CaptureOrdinal 1
    c2 = IsCaptureOrdinal $ CaptureOrdinal 2
\end{code}

\begin{code}
fixpoint :: (Eq a) => (a->a) -> a -> a
fixpoint f = chk . iterate f
  where
    chk (x:x':_) | x==x' = x
    chk xs               = chk $ tail xs
\end{code}

\begin{code}
($=~) :: ( RegexContext Regex source target
         , RegexMaker   Regex CompOption ExecOption String
         )
      => source -> String -> target
($=~) = (=~)

\end{code}
