\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}

module Text.RE.Types.Options where

import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import           Data.String
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
\end{code}

\begin{code}
data Options_ r c e =
  Options
    { optionsMacs :: !(Macros r)
    , optionsComp :: !c
    , optionsExec :: !e
    }
  deriving (Show)
\end{code}

\begin{code}
class IsOption o r c e |
    e -> r, c -> e , e -> c, r -> c, c -> r, r -> e where
  makeOptions :: o -> Options_ r c e
\end{code}

\begin{code}
newtype MacroID =
    MacroID { getMacroID :: String }
  deriving (IsString,Ord,Eq,Show)
\end{code}

\begin{code}
instance Hashable MacroID where
  hashWithSalt i = hashWithSalt i . getMacroID
\end{code}

\begin{code}
type Macros r = HM.HashMap MacroID r
\end{code}

\begin{code}
emptyMacros :: Macros r
emptyMacros = HM.empty
\end{code}

\begin{code}
data SimpleRegexOptions
  = MultilineSensitive
  | MultilineInsensitive
  | BlockSensitive
  | BlockInsensitive
  deriving (Bounded,Enum,Eq,Ord,Show)
\end{code}

\begin{code}
instance Lift SimpleRegexOptions where
  lift sro = case sro of
    MultilineSensitive    -> conE 'MultilineSensitive
    MultilineInsensitive  -> conE 'MultilineInsensitive
    BlockSensitive        -> conE 'BlockSensitive
    BlockInsensitive      -> conE 'BlockInsensitive
\end{code}
