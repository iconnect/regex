\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Text.RE.Tools.Edit
  (
  -- * Editing
  -- $tutorial
    Edits(..)
  , Edit(..)
  , LineEdit(..)
  , applyEdits
  , applyEdit
  , applyLineEdit
  -- * IsRegex
  , IsRegex(..)
  , SearchReplace(..)
  , searchReplaceAll
  , searchReplaceFirst
  -- * LineNo
  , LineNo(..)
  , firstLine
  , getLineNo
  , lineNo
  -- * Replace
  , module Text.RE.Replace
  ) where

import           Data.Maybe
import           Prelude.Compat
import           Text.RE.Replace
import           Text.RE.Tools.IsRegex
import           Text.RE.ZeInternals.Types.LineNo
\end{code}


\begin{code}
-- | an 'Edits' script will, for each line in the file, either perform
-- the action selected by the first RE in the list, or perform all of the
-- actions on line, arranged as a pipeline
data Edits m re s
  = Select ![Edit m re s]   -- ^ for each line select the first @Edit@ to match each line and edit the line with it
  | Pipe   ![Edit m re s]   -- ^ for each line apply every edit that matches in turn to the line

-- | each Edit action specifies how the match should be processed
data Edit m re s
  = Template !(SearchReplace re s)
        -- ^ replace the match with this template text, substituting ${capture} as apropriate
  | Function !re REContext !(LineNo->Match s->RELocation->Capture s->m (Maybe s))
        -- ^ use this function to replace the 'REContext' specified captures in each line matched
  | LineEdit !re           !(LineNo->Matches s->m (LineEdit s))
        -- ^ use this function to edit each line matched

-- | a LineEdit is the most general action thar can be performed on a line
-- and is the only means of deleting a line
data LineEdit s
  = NoEdit                  -- ^ do not edit this line but leave as is
  | ReplaceWith !s          -- ^ replace the line with this text (terminating newline should not be included)
  | Delete                  -- ^ delete the this line altogether
  deriving (Functor,Show)
\end{code}


\begin{code}
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
          -> Edit m re s
          -> s
          -> m (Maybe s)
applyEdit anl lno edit s =
  case allMatches acs of
    [] -> return Nothing
    _  -> fmap Just $ case edit of
      Template srch_rpl -> return $ anl $ replaceAll (getTemplate srch_rpl)       acs
      Function _ ctx f  -> anl <$> replaceAllCapturesM replaceMethods ctx (f lno) acs
      LineEdit _     g  -> fromMaybe (anl s) . applyLineEdit anl <$> g lno        acs
  where
    acs = matchMany rex s
    rex = case edit of
      Template srch_rpl -> getSearch srch_rpl
      Function rex_ _ _ -> rex_
      LineEdit rex_   _ -> rex_


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
                    -> [Edit m re s]
                    -> s
                    -> m s
select_edit_scripts lno ps0 s = select ps0
  where
    select []           = return $ appendNewlineR s
    select (edit:edits) =
      applyEdit appendNewlineR lno edit s >>= maybe (select edits) return

pipe_edit_scripts :: (IsRegex re s,Monad m,Functor m)
                  => LineNo
                  -> [Edit m re s]
                  -> s
                  -> m s
pipe_edit_scripts lno edits s0 =
    appendNewlineR <$> foldr f (return s0) edits
  where
    f edit act = do
      s <- act
      fromMaybe s <$> applyEdit id lno edit s
\end{code}

\begin{code}
-- $tutorial
-- The Edit toolkit looks for REs that match a text and runs the
-- associated actions.
--
-- See the Regex Tools tutorial at http://re-tutorial-tools.regex.uk
\end{code}
