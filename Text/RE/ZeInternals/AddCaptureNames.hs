{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Text.RE.ZeInternals.AddCaptureNames where

import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Data.ByteString.Char8         as B
import           Data.Dynamic
import           Data.Maybe
import qualified Data.Sequence                 as S
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Prelude.Compat
import           Text.RE.ZeInternals.Types.CaptureID
import           Text.RE.ZeInternals.Types.Match
import           Text.RE.ZeInternals.Types.Matches
import           Unsafe.Coerce


-- | a convenience function used by the API modules to insert
-- capture names extracted from the parsed RE into the (*=~) result
addCaptureNamesToMatches :: CaptureNames -> Matches a -> Matches a
addCaptureNamesToMatches cnms mtchs =
  mtchs { allMatches = map (addCaptureNamesToMatch cnms) $ allMatches mtchs }

-- | a convenience function used by the API modules to insert
-- capture names extracted from the parsed RE into the (?=~) result
addCaptureNamesToMatch :: CaptureNames -> Match a -> Match a
addCaptureNamesToMatch cnms mtch = mtch { captureNames = cnms }

-- | a hairy dynamically-typed function used with the legacy (=~) and (=~~)
-- to see if it can/should add the capture names extracted from the RE
-- into the polymorphic result of the operator (it does for any Match
-- or Matches type, provided it is parameterised over a recognised type).
-- The test suite is all over this one, testing all of these cases.
addCaptureNames :: Typeable a => CaptureNames -> a -> a
addCaptureNames cnms x = fromMaybe x $ listToMaybe $ catMaybes
    [ test_match   x ( proxy :: String         )
    , test_matches x ( proxy :: String         )
    , test_match   x ( proxy :: B.ByteString   )
    , test_matches x ( proxy :: B.ByteString   )
    , test_match   x ( proxy :: LBS.ByteString )
    , test_matches x ( proxy :: LBS.ByteString )
    , test_match   x ( proxy :: T.Text         )
    , test_matches x ( proxy :: T.Text         )
    , test_match   x ( proxy :: TL.Text        )
    , test_matches x ( proxy :: TL.Text        )
    , test_match   x ( proxy :: S.Seq Char     )
    , test_matches x ( proxy :: S.Seq Char     )
    ]
  where
    test_match :: Typeable t => r -> t -> Maybe r
    test_match r t = f r t $ addCaptureNamesToMatch cnms <$> fromDynamic dyn
      where
        f :: r' -> t' -> Maybe (Match t') -> Maybe r'
        f _ _ = unsafeCoerce

    test_matches :: Typeable t => r -> t -> Maybe r
    test_matches r t = f r t $ addCaptureNamesToMatches cnms <$> fromDynamic dyn
      where
        f :: r' -> t' -> Maybe (Matches t') -> Maybe r'
        f _ _ = unsafeCoerce

    dyn :: Dynamic
    dyn = toDyn x

    proxy :: a
    proxy = error "addCaptureNames"
