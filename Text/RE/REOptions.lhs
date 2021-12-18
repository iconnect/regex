\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes      #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
#else
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
#endif

module Text.RE.REOptions
  (
  -- * The Options Tutorial
  -- $tutorial

  -- * 'SimpleREOptions'
    SimpleREOptions(..)
  -- * 'REOptions_'
  , REOptions_(..)
  -- * The Macro Tables
  , Macros
  , MacroID(..)
  , emptyMacros
  ) where

import qualified Data.HashMap.Strict        as HM
import           Data.Hashable
import           Data.String
import           Language.Haskell.TH.Syntax
\end{code}


The RE Options
--------------

\begin{code}
-- | the default API uses these simple, universal RE options,
-- which get auto-converted into the apropriate back-end 'REOptions_'
data SimpleREOptions
  = MultilineSensitive        -- ^ case-sensitive with ^ and $ matching the start and end of a line
  | MultilineInsensitive      -- ^ case-insensitive with ^ and $ matsh the start and end of a line
  | BlockSensitive            -- ^ case-sensitive with ^ and $ matching the start and end of the input text
  | BlockInsensitive          -- ^ case-insensitive with ^ and $ matching the start and end of the input text
  deriving (Bounded,Enum,Eq,Ord,Show)
\end{code}

\begin{code}
-- | we need to use this in the quasi quoters to specify @SimpleREOptions@
-- selected by the quasi quoter
deriving instance Lift SimpleREOptions
\end{code}

\begin{code}
-- | the general options for an RE are dependent on which back end is
-- being used and are parameterised over the @RE@ type for the back end,
-- and its @CompOption@ and @ExecOption@ types (the compile-time and
-- execution time options, respectively); each back end will define an
-- @REOptions@ type that fills out these three type parameters with the
-- apropriate types (see, for example, "Text.RE.TDFA")
data REOptions_ r c e =
  REOptions
    { optionsMacs :: !(Macros r)    -- ^ the available TestBench RE macros
    , optionsComp :: !c             -- ^ the back end compile-time options
    , optionsExec :: !e             -- ^ the back end execution-time options
    }
  deriving (Show)
\end{code}


The Macro Tables
----------------

\begin{code}
-- | our macro tables are parameterised over the back end @RE@ type and
-- and just associate each @MacroID@ with an @RE@ (which may in turn
-- contain macros to be expanded)
type Macros r = HM.HashMap MacroID r
\end{code}

\begin{code}
-- | @MacroID@ is just a wrapped @String@ type with an @IsString@
-- instance
newtype MacroID =
    MacroID { getMacroID :: String }
  deriving (IsString,Ord,Eq,Show)
\end{code}

\begin{code}
-- | @MacroID@ is used with @HM.HashMap@ to build macro lookup tables
instance Hashable MacroID where
  hashWithSalt i = hashWithSalt i . getMacroID
\end{code}

\begin{code}
-- | a macro table containing no entries
emptyMacros :: Macros r
emptyMacros = HM.empty
\end{code}


\begin{code}
-- $tutorial
-- This API module provides the generic types used to specify the options
-- when compiling REs for each of the backl ends.
--
-- See the tutorials at http://re-tutorial-options.regex.uk
\end{code}
