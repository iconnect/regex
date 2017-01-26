\begin{code}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}

module Text.RE.Replace
  ( Replace(..)
  , Replace_(..)
  , replace_
  , Phi(..)
  , Context(..)
  , Location(..)
  , isTopLocation
  , replace
  , replaceAll
  , replaceAllCaptures
  , replaceAllCaptures'
  , replaceAllCaptures_
  , replaceAllCapturesM
  , replaceCaptures
  , replaceCaptures'
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
import           Text.Heredoc
import           Text.RE.Capture
import           Text.RE.CaptureID
import           Text.RE.Options
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Text()
import           Text.Regex.TDFA.Text.Lazy()
\end{code}

\begin{code}
-- | Replace provides the missing methods needed to replace the matched
-- text; length_ is the minimum implementation
class (Extract a,Monoid a) => Replace a where
  -- | length function for a
  length_       :: a -> Int
  -- | inject String into a
  pack_         :: String -> a
  -- | project a onto a String
  unpack_       :: a -> String
  -- | inject into Text
  textify       :: a -> T.Text
  -- | project Text onto a
  detextify     :: T.Text -> a
  -- | append a newline
  appendNewline :: a -> a
  -- | apply a substitution function to a Capture
  subst         :: (a->a) -> Capture a -> a
  -- | convert a template containing $0, $1, etc., in the first
  -- argument, into a 'phi' replacement function for use with
  -- replaceAllCaptures' and replaceCaptures'
  parse_tpl     :: a -> Match a -> Location -> Capture a -> Maybe a

  textify       = T.pack . unpack_
  detextify     = pack_  . T.unpack
  appendNewline = (<> pack_ "\n")

  subst f m@Capture{..} =
    capturePrefix m <> f capturedText <> captureSuffix m
\end{code}

\begin{code}
-- | a selction of the Replace methods can be encapsulated with Replace_
-- for the higher-order replacement functions
data Replace_ a =
  Replace_
    { _r_length :: a -> Int
    , _r_subst  :: (a->a) -> Capture a -> a
    }

-- | replace_ encapsulates Replace_ a from a Replace a context
replace_ :: Replace a => Replace_ a
replace_ =
  Replace_
    { _r_length = length_
    , _r_subst  = subst
    }
\end{code}

\begin{code}
-- | @Phi@ specifies the substitution function for procesing the substrings
-- captured by the regular expression.
data Phi a =
  Phi
    { _phi_context :: Context             -- ^ the context for applying
                                          -- the substitution
    , _phi_phi     :: Location -> a -> a  -- ^ the substitution function
                                          -- takes the location and
                                          -- the text to be replaced and
                                          -- returns the replacement
                                          -- text to be substituted
    }

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
    { _loc_match   :: Int   -- ^ the zero-based, i-th string to be
                            -- matched, when matching all strings,
                            -- zero when only the first string is
                            -- being matched
    , _loc_capture :: CaptureOrdinal
                            -- ^ 0, when matching the top-level
                            -- string matched by the whole RE, 1
                            -- for the top-most, left-most redex
                            -- captured by bracketed sub-REs, etc.
    }
  deriving (Show)
\end{code}

\begin{code}
-- | True iff the location references a complete match
-- (i.e., not a bracketed capture)
isTopLocation :: Location -> Bool
isTopLocation = (==0) . _loc_capture
\end{code}

\begin{code}
-- | replace all with a template, $0 for whole text, $1 for first
-- capture, etc.
replaceAll :: Replace a
           => a
           -> Matches a
           -> a
replaceAll tpl ac = replaceAllCaptures' TOP (parse_tpl tpl) ac
\end{code}

\begin{code}
-- | substitutes the PHI substitutions through the Matches
replaceAllCaptures :: Replace a
                   => Phi a
                   -> Matches a
                   -> a
replaceAllCaptures = mk_phi replaceAllCaptures'
\end{code}

\begin{code}
-- | substitutes using a function that takes the full Match
-- context and returns the same replacement text as the _phi_phi
-- context.
replaceAllCaptures' :: Replace a
                    => Context
                    -> (Match a->Location->Capture a->Maybe a)
                    -> Matches a
                    -> a
\end{code}

\begin{code}
replaceAllCaptures' = replaceAllCaptures_ replace_
\end{code}

\begin{code}
-- | replaceAllCaptures_ is like like replaceAllCaptures' but takes the
-- Replace methods through the Replace_ argument
replaceAllCaptures_ :: Extract a
                    => Replace_ a
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
                    => Replace_ a
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
replace c tpl = replaceCaptures' TOP (parse_tpl tpl) c
\end{code}

\begin{code}
-- | substitutes the PHI substitutions through the Match
replaceCaptures :: Replace a
                => Phi a
                -> Match a
                -> a
replaceCaptures = mk_phi replaceCaptures'
\end{code}

\begin{code}
-- | substitutes using a function that takes the full Match
-- context and returns the same replacement text as the _phi_phi
-- context.
replaceCaptures' :: Replace a
                 => Context
                 -> (Match a->Location->Capture a->Maybe a)
                 -> Match a
                 -> a
replaceCaptures' = replaceCaptures_ replace_
\end{code}

\begin{code}
-- | replaceCaptures_ is like replaceCaptures' but takes the Replace methods
-- through the Replace_ argument
replaceCaptures_ :: Extract a
                 => Replace_ a
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
                 => Replace_ a
                 -> Context
                 -> (Match a->Location->Capture a->m (Maybe a))
                 -> Match a
                 -> m a
replaceCapturesM Replace_{..} ctx phi_ caps@Match{..} = do
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
              ( _r_subst (const ndl') cap
              , (captureOffset cap,len'-len) : ds
              )
          where
            len' = _r_length ndl'
            len  = _r_length ndl

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
  length_       = length
  pack_         = id
  unpack_       = id
  textify       = T.pack
  detextify     = T.unpack
  appendNewline = (<>"\n")
  parse_tpl     = parse_tpl_ id

instance Replace B.ByteString where
  length_   = B.length
  pack_     = B.pack
  unpack_   = B.unpack
  textify   = TE.decodeUtf8
  detextify = TE.encodeUtf8
  appendNewline = (<>"\n")
  parse_tpl = parse_tpl_ B.unpack

instance Replace LBS.ByteString where
  length_   = fromEnum . LBS.length
  pack_     = LBS.pack
  unpack_   = LBS.unpack
  textify   = TE.decodeUtf8  . LBS.toStrict
  detextify = LBS.fromStrict . TE.encodeUtf8
  appendNewline = (<>"\n")
  parse_tpl = parse_tpl_ LBS.unpack

instance Replace (S.Seq Char) where
  length_   = S.length
  pack_     = S.fromList
  unpack_   = F.toList
  parse_tpl = parse_tpl_ F.toList

instance Replace T.Text where
  length_   = T.length
  pack_     = T.pack
  unpack_   = T.unpack
  textify   = id
  detextify = id
  appendNewline = (<>"\n")
  parse_tpl = parse_tpl_ T.unpack

instance Replace LT.Text where
  length_   = fromEnum . LT.length
  pack_     = LT.pack
  unpack_   = LT.unpack
  textify   = LT.toStrict
  detextify = LT.fromStrict
  appendNewline = (<>"\n")
  parse_tpl = parse_tpl_ LT.unpack
\end{code}

\begin{code}
-- | expand all of the @{..} macros in the RE in the argument String
-- according to the Macros argument, preprocessing the RE String
-- according to the Mode argument (used internally)
expandMacros :: (r->String) -> Mode -> Macros r -> String -> String
expandMacros x_src md hm s0 =
  case HM.null hm of
    True  -> s
    False -> expandMacros' (fmap x_src . flip HM.lookup hm) s
  where
    s = case md of
      Simple -> s0
      Block  -> concat $ map clean $ lines s0

    clean = reverse . dropWhile isSpace . reverse . dropWhile isSpace
\end{code}

\begin{code}
-- | expand the @{..} macos in the argument string using the given
-- function
expandMacros' :: (MacroID->Maybe String) -> String -> String
expandMacros' lu = fixpoint e_m
  where
    e_m re_s = replaceAllCaptures' TOP phi $ re_s $=~ [here|@(@|\{([^{}]+)\})|]
      where
        phi mtch _ cap = case txt == "@@" of
            True  -> Just   "@"
            False -> Just $ fromMaybe txt $ lu ide
          where
            txt = capturedText cap
            ide = MacroID $ capturedText $ capture c2 mtch
            c2  = CID_ordinal $ CaptureOrdinal 2
\end{code}

\begin{code}
lift_phi :: Monad m
         => (Match a->Location->Capture a->Maybe a)
         -> (Match a->Location->Capture a->m (Maybe a))
lift_phi phi_ = phi
  where
    phi caps' loc' cap' = return $ phi_ caps' loc' cap'

mk_phi :: (Context->(Match a->Location->Capture a->Maybe a)->b)
       -> Phi a
       -> b
mk_phi f phi@Phi{..} = f _phi_context $ mk_phi' phi

mk_phi' :: Phi a -> Match a -> Location -> Capture a -> Maybe a
mk_phi' Phi{..} _ loc = Just . _phi_phi loc . capturedText
\end{code}

\begin{code}
parse_tpl_ :: ( Replace a
              , RegexContext Regex a (Matches a)
              , RegexMaker   Regex CompOption ExecOption String
              )
           => (a->String)
           -> a
           -> Match a
           -> Location
           -> Capture a
           -> Maybe a
parse_tpl_ unpack tpl mtch _ _ =
    Just $ replaceAllCaptures' TOP phi $
      tpl $=~ [here|\$(\$|[0-9]+|\{([^{}]+)\})|]
  where
    phi t_mtch _ _ = case captureMaybe c2 t_mtch of
      Just cap -> this $ CID_name $ CaptureName txt
        where
          txt = T.pack $ unpack $ capturedText cap
      Nothing -> case s == "$" of
        True  -> Just t
        False -> this $ CID_ordinal $ CaptureOrdinal $ read s
      where
        s  = unpack t
        t  = capturedText $ capture c1 t_mtch

        this cid = capturedText <$> captureMaybe cid mtch

    c1 = CID_ordinal $ CaptureOrdinal 1
    c2 = CID_ordinal $ CaptureOrdinal 2
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
