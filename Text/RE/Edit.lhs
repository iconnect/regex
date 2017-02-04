\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.Edit
  ( LineNo
  , Edits(..)
  , Edit(..)
  , LineEdit(..)
  , applyEdits
  , applyEdit
  , applyLineEdit
  ) where

import           Data.Maybe
import           Prelude.Compat
import           Text.RE.Capture
import           Text.RE.IsRegex
import           Text.RE.LineNo
import           Text.RE.Replace


data Edits m re s
  = Select [(re,Edit m s)]
  | Pipe   [(re,Edit m s)]

data Edit m s
  = EDIT_tpl s
  | EDIT_phi (Phi s)
  | EDIT_fun Context (LineNo->Match s->Location->Capture s->m (Maybe s))
  | EDIT_gen         (LineNo->Matches s->m (LineEdit s))

data LineEdit s
  = NoEdit
  | ReplaceWith s
  | Delete
  deriving (Show)

applyEdits :: (IsRegex re s,Monad m,Functor m)
           => LineNo
           -> Edits m re s
           -> s
           -> m s
applyEdits lno ez0 s0 = case ez0 of
  Select ez -> select_edit_scripts lno ez s0
  Pipe   ez -> pipe_edit_scripts   lno ez s0

applyEdit :: (IsRegex re s,Monad m,Functor m)
          => (s->s)
          -> LineNo
          -> re
          -> Edit m s
          -> s
          -> m (Maybe s)
applyEdit anl lno re edit s =
  case allMatches acs of
    [] -> return Nothing
    _  -> fmap Just $ case edit of
      EDIT_tpl tpl   -> return $ anl $ replaceAll         tpl acs
      EDIT_phi phi   -> return $ anl $ replaceAllCaptures phi acs
      EDIT_fun ctx f -> anl <$> replaceAllCapturesM replace_ ctx (f lno) acs
      EDIT_gen     g -> fromMaybe s' . applyLineEdit anl <$> g lno acs
  where
    s'  = anl s
    acs = matchMany re s

applyLineEdit :: Monoid s => (s->s) -> LineEdit s -> Maybe s
applyLineEdit _    NoEdit         = Nothing
applyLineEdit anl (ReplaceWith s) = Just $ anl s
applyLineEdit _    Delete         = Just $ mempty

select_edit_scripts :: (IsRegex re s,Monad m,Functor m)
                    => LineNo
                    -> [(re,Edit m s)]
                    -> s
                    -> m s
select_edit_scripts lno ps0 s = select ps0
  where
    select []           = return $ appendNewline s
    select ((re,es):ps) =
      applyEdit appendNewline lno re es s >>= maybe (select ps) return

pipe_edit_scripts :: (IsRegex re s,Monad m,Functor m)
                  => LineNo
                  -> [(re,Edit m s)]
                  -> s
                  -> m s
pipe_edit_scripts lno ez s0 =
    appendNewline <$> foldr f (return s0) ez
  where
    f (re,es) act = do
      s <- act
      fromMaybe s <$> applyEdit id lno re es s
\end{code}
