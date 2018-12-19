\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MonoLocalBinds             #-}

module Text.RE.ZeInternals.Replace
  (
  -- * REContext and RELocation
    REContext(..)
  , RELocation(..)
  , isTopLocation
  -- * replaceAll
  , replaceAll
  , replaceAllCaptures
  , replaceAllCaptures_
  , replaceAllCapturesM
  -- * replace
  , replace
  , replaceCaptures
  , replaceCaptures_
  , replaceCapturesM
  -- * expandMacros
  , expandMacros
  , expandMacros'
  -- * templateCaptures
  , templateCaptures
  -- * Replace and ReplaceMethods
  , Replace(..)
  , ReplaceMethods(..)
  , replaceMethods
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
import qualified Data.Monoid                    as M
import qualified Data.Sequence                  as S
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Text.Lazy                 as LT
import           Prelude.Compat
import           Text.RE.REOptions
import           Text.RE.ZeInternals.Types.Capture
import           Text.RE.ZeInternals.Types.CaptureID
import           Text.RE.ZeInternals.Types.Match
import           Text.RE.ZeInternals.Types.Matches
import           Text.Read
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Text()
import           Text.Regex.TDFA.Text.Lazy()
\end{code}


ReContext and RELocation
------------------------

\begin{code}
-- | @REContext@ specifies which contexts the substitutions should be applied
data REContext
  = TOP   -- ^ substitutions should be applied to the top-level only,
          -- the text that matched the whole RE
  | SUB   -- ^ substitutions should only be applied to the text
          -- captured by bracketed sub-REs
  | ALL   -- ^ the substitution function should be applied to all
          -- captures, the top level and the sub-expression captures
  deriving (Show)

-- | the @RELocation@ information passed into the substitution function
-- specifies which sub-expression is being substituted
data RELocation =
  RELocation
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
isTopLocation :: RELocation -> Bool
isTopLocation = (==0) . locationCapture
\end{code}

\begin{code}
-- | replace all with a template, $0 for whole text, $1 for first
-- capture, etc.
replaceAll :: Replace a
           => a
           -> Matches a
           -> a
replaceAll tpl ac = replaceAllCaptures TOP (parseTemplateR tpl) ac
\end{code}

\begin{code}
-- | substitutes using a function that takes the full Match
-- context and returns the same replacement text as the _phi_phi
-- context.
replaceAllCaptures :: Replace a
                   => REContext
                   -> (Match a->RELocation->Capture a->Maybe a)
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
                    -> REContext
                    -> (Match a->RELocation->Capture a->Maybe a)
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
                    -> REContext
                    -> (Match a->RELocation->Capture a->m (Maybe a))
                    -> Matches a
                    -> m a
replaceAllCapturesM r ctx phi_ Matches{..} =
    replaceCapturesM r ALL phi $ Match matchesSource cnms arr
  where
    phi _ (RELocation _ i) = case arr_c!i of
      Just caps -> phi_ caps . uncurry RELocation $ arr_i ! i
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
        => a
        -> Match a
        -> a
replace tpl c = replaceCaptures TOP (parseTemplateR tpl) c
\end{code}

\begin{code}
-- | substitutes using a function that takes the full Match
-- context and returns the same replacement text as the _phi_phi
-- context.
replaceCaptures :: Replace a
                 => REContext
                 -> (Match a->RELocation->Capture a->Maybe a)
                 -> Match a
                 -> a
replaceCaptures = replaceCaptures_ replaceMethods
\end{code}

\begin{code}
-- | replaceCaptures_ is like replaceCaptures but takes the Replace methods
-- through the ReplaceMethods argument
replaceCaptures_ :: Extract a
                 => ReplaceMethods a
                 -> REContext
                 -> (Match a->RELocation->Capture a->Maybe a)
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
                 -> REContext
                 -> (Match a->RELocation->Capture a->m (Maybe a))
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
          False -> phi_ caps (RELocation 0 i) cap
\end{code}

expandMacros
------------

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
    e_m re_s = replaceAllCaptures TOP phi $ re_s $=~ "@(@|\\{([^{}]+)\\})"
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
         => (Match a->RELocation->Capture a->Maybe a)
         -> (Match a->RELocation->Capture a->m (Maybe a))
lift_phi phi_ = phi
  where
    phi caps' loc' cap' = return $ phi_ caps' loc' cap'
\end{code}


templateCaptures
----------------

\begin{code}
-- | list all of the CaptureID references in the replace template in
-- the second argument
templateCaptures :: ( Replace a
                    , RegexContext Regex a (Matches a)
                    , RegexMaker   Regex CompOption ExecOption String
                    )
                 => (a->String)
                 -> a
                 -> [CaptureID]
templateCaptures unpack tpl =
    [ cid
      | mtch <- allMatches $ scan_template tpl
      , Right cid <- [parse_template_capture unpack mtch]
      ]

-- | parse a Match generated by acan_template, returning @Left "$")
-- iff the capture reference is an escaped @$@ (i.e., @$$@)
parse_template_capture :: (a->String) -> Match a -> Either a CaptureID
parse_template_capture unpack t_mtch = case t_mtch !$? c2 of
  Just cap -> case readMaybe stg of
      Nothing -> Right $ IsCaptureName    $ CaptureName $ T.pack stg
      Just cn -> Right $ IsCaptureOrdinal $ CaptureOrdinal cn
    where
      stg = unpack $ capturedText cap
  Nothing -> case s == "$" of
    True  -> Left t
    False -> Right $ IsCaptureOrdinal $ CaptureOrdinal $ read s
  where
    s = unpack t
    t = capturedText $ capture c1 t_mtch

    c1 = IsCaptureOrdinal $ CaptureOrdinal 1
    c2 = IsCaptureOrdinal $ CaptureOrdinal 2

-- | scan a replacement template, returning a Match for each capture
-- reference in the template (like $1, ${foo})
scan_template :: ( Replace a
                 , RegexContext Regex a (Matches a)
                 , RegexMaker   Regex CompOption ExecOption String
                 )
              => a
              -> Matches a
scan_template tpl = tpl $=~ "\\$(\\$|[0-9]|\\{([^{}]+)\\})"
\end{code}


Replace and ReplaceMethods
--------------------------

\begin{code}
-- | Replace provides the missing needed to replace the matched
-- text in a @Replace a => Match a@.
class (Show a,Eq a,Ord a,Extract a,Monoid a) => Replace a where
  -- | length function for a
  lengthR        :: a -> Int
  -- | inject String into a
  packR          :: String -> a
  -- | project a onto a String
  unpackR        :: a -> String
  -- | inject into Text
  textifyR       :: a -> T.Text
  -- | project Text onto a
  detextifyR     :: T.Text -> a
  -- | split into lines
  linesR         :: a -> [a]
  -- | concatenate a list of lines
  unlinesR       :: [a] -> a
  -- | append a newline
  appendNewlineR :: a -> a
  -- | apply a substitution function to a Capture
  substR         :: (a->a) -> Capture a -> a
  -- | convert a template containing $0, $1, etc., in the first
  -- argument, into a 'phi' replacement function for use with
  -- replaceAllCaptures and replaceCaptures
  parseTemplateR :: a -> Match a -> RELocation -> Capture a -> Maybe a

  textifyR       = T.pack . unpackR
  detextifyR     = packR  . T.unpack
  appendNewlineR = (M.<> packR "\n")

  substR f m@Capture{..} =
    capturePrefix m M.<> f capturedText M.<> captureSuffix m
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
    { methodLength = lengthR
    , methodSubst  = substR
    }
\end{code}


The Replace Instances
---------------------

\begin{code}
instance Replace [Char] where
  lengthR         = length
  packR           = id
  unpackR         = id
  textifyR        = T.pack
  detextifyR      = T.unpack
  linesR          = lines
  unlinesR        = unlines
  appendNewlineR  = (M.<>"\n")
  parseTemplateR  = parseTemplateR' id

instance Replace B.ByteString where
  lengthR         = B.length
  packR           = B.pack
  unpackR         = B.unpack
  textifyR        = TE.decodeUtf8
  detextifyR      = TE.encodeUtf8
  linesR          = B.lines
  unlinesR        = B.unlines
  appendNewlineR  = (M.<>"\n")
  parseTemplateR  = parseTemplateR' B.unpack

instance Replace LBS.ByteString where
  lengthR         = fromEnum . LBS.length
  packR           = LBS.pack
  unpackR         = LBS.unpack
  textifyR        = TE.decodeUtf8  . LBS.toStrict
  linesR          = LBS.lines
  unlinesR        = LBS.unlines
  detextifyR      = LBS.fromStrict . TE.encodeUtf8
  appendNewlineR  = (M.<>"\n")
  parseTemplateR  = parseTemplateR' LBS.unpack

instance Replace (S.Seq Char) where
  lengthR         = S.length
  packR           = S.fromList
  unpackR         = F.toList
  linesR          = map packR . lines . unpackR
  unlinesR        = packR . unlines . map unpackR
  parseTemplateR  = parseTemplateR' F.toList

instance Replace T.Text where
  lengthR         = T.length
  packR           = T.pack
  unpackR         = T.unpack
  textifyR        = id
  detextifyR      = id
  linesR          = T.lines
  unlinesR        = T.unlines
  appendNewlineR  = (M.<>"\n")
  parseTemplateR  = parseTemplateR' T.unpack

instance Replace LT.Text where
  lengthR         = fromEnum . LT.length
  packR           = LT.pack
  unpackR         = LT.unpack
  textifyR        = LT.toStrict
  detextifyR      = LT.fromStrict
  linesR          = LT.lines
  unlinesR        = LT.unlines
  appendNewlineR  = (M.<>"\n")
  parseTemplateR  = parseTemplateR' LT.unpack
\end{code}


Parsing Replace Templates
-------------------------

\begin{code}
-- | parse the replacement template in second argument, substititing
-- the capture references with corresponding captures from the Match
-- in the third argument (the result of a single match of the RE
-- against the input text to be matched); Nothing is returned if the
-- inputs are not well formed (currently all inputs are well formed)
parseTemplateR' :: ( Replace a
                   , RegexContext Regex a (Matches a)
                   , RegexMaker   Regex CompOption ExecOption String
                   )
                   => (a->String)
                   -> a
                   -> Match a
                   -> RELocation
                   -> Capture a
                   -> Maybe a
parseTemplateR' unpack tpl mtch _ _ =
    Just $ replaceAllCaptures TOP phi $ scan_template tpl
  where
    phi t_mtch _ _ = either Just this $ parse_template_capture unpack t_mtch

    this cid       = capturedText <$> mtch !$? cid
\end{code}


Helpers
-------

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
