\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.Tools.Edit
  ( Edits(..)
  , Edit(..)
  , LineEdit(..)
  , applyEdits
  , applyEdit
  , applyLineEdit
  -- * LineNo
  , LineNo(..)
  , firstLine
  , getLineNo
  , lineNo
  -- * Text.RE
  , module Text.RE
  ) where

import           Data.Maybe
import           Prelude.Compat
import           Text.RE
import           Text.RE.Types.IsRegex
import           Text.RE.Types.LineNo


-- | an 'Edits' script will, for each line in the file, either perform
-- the action selected by the first RE in the list, or perform all of the
-- actions on line, arranged as a pipeline
data Edits m re s
  = Select [(re,Edit m s)]
  | Pipe   [(re,Edit m s)]

-- | each Edit action specifies how the match should be processed
data Edit m s
  = Template s
  | Function Context (LineNo->Match s->Location->Capture s->m (Maybe s))
  | LineEdit         (LineNo->Matches s->m (LineEdit s))

-- | a LineEdit is the most general action thar can be performed on a line
-- and is the only means of deleting a line
data LineEdit s
  = NoEdit
  | ReplaceWith s
  | Delete
  deriving (Show)

-- | apply an 'Edit' script to a single line
applyEdits :: (IsRegex re s,Monad m,Functor m)
           => LineNo
           -> Edits m re s
           -> s
           -> m s
applyEdits lno ez0 s0 = case ez0 of
  Select ez -> select_edit_scripts lno ez s0
  Pipe   ez -> pipe_edit_scripts   lno ez s0

-- | apply a single edit action to a line, the function in the first argument
-- being used to add a new line onto the end of the line where appropriate;
-- the function returns @Nothing@ if no edit is to be performed on the line,
-- @Just mempty@ to delete the line
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
      Template tpl   -> return $ anl $ replaceAll         tpl acs
      Function ctx f -> anl <$> replaceAllCapturesM replaceMethods ctx (f lno) acs
      LineEdit     g -> fromMaybe s' . applyLineEdit anl <$> g lno acs
  where
    s'  = anl s
    acs = matchMany re s

-- | apply a 'LineEdit' to a line, using the function in the first
-- argument to append a new line to the result; Nothing should be
-- returned if no edit is to be performed,  @Just mempty@ to
-- delete the line
applyLineEdit :: Monoid s => (s->s) -> LineEdit s -> Maybe s
applyLineEdit _    NoEdit         = Nothing
applyLineEdit anl (ReplaceWith s) = Just $ anl s
applyLineEdit _    Delete         = Just   mempty

select_edit_scripts :: (IsRegex re s,Monad m,Functor m)
                    => LineNo
                    -> [(re,Edit m s)]
                    -> s
                    -> m s
select_edit_scripts lno ps0 s = select ps0
  where
    select []           = return $ appendNewlineE s
    select ((re,es):ps) =
      applyEdit appendNewlineE lno re es s >>= maybe (select ps) return

pipe_edit_scripts :: (IsRegex re s,Monad m,Functor m)
                  => LineNo
                  -> [(re,Edit m s)]
                  -> s
                  -> m s
pipe_edit_scripts lno ez s0 =
    appendNewlineE <$> foldr f (return s0) ez
  where
    f (re,es) act = do
      s <- act
      fromMaybe s <$> applyEdit id lno re es s
\end{code}
