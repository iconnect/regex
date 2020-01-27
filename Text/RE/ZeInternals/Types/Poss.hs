{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

module Text.RE.ZeInternals.Types.Poss where

import           Control.Monad.Fail


data Poss a
  = Eek String
  | Yup a
  deriving (Eq,Ord,Show)

instance Functor Poss where
  fmap f p = case p of
    Eek m -> Eek m
    Yup x -> Yup $ f x

instance Applicative Poss where
  pure        = Yup
  (<*>) p1 p2 = case p1 of
    Eek m -> Eek m
    Yup f -> case p2 of
      Eek n -> Eek n
      Yup x -> Yup $ f x

instance Monad Poss where
  return = pure
  (>>=) p f = case p of
    Eek m -> Eek m
    Yup x -> f x

instance MonadFail Poss where
  fail = Eek

poss :: (String->b) -> (a->b) -> Poss a -> b
poss f _ (Eek s) = f s
poss _ g (Yup x) = g x

poss2either :: Poss a -> Either String a
poss2either (Eek m) = Left  m
poss2either (Yup x) = Right x
