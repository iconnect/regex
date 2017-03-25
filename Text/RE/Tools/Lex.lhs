\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}

module Text.RE.Tools.Lex
  ( alex
  , alex'
  -- * IsRegex
  , IsRegex(..)
  -- * Text.RE
  , module Text.RE
  ) where

import           Prelude.Compat
import           Text.RE
import           Text.RE.Types.Capture
import           Text.RE.Types.IsRegex
import           Text.RE.Types.Match
import           Text.RE.Types.Replace


-- | a simple regex-based scanner interpretter for prototyping
-- scanners
alex :: IsRegex re s => [(re,Match s->Maybe t)] -> t -> s -> [t]
alex = alex' matchOnce

-- | a higher order version of 'alex' parameterised over the @matchOnce@
-- function
alex' :: Replace s
      => (re->s->Match s)
      -> [(re,Match s->Maybe t)]
      -> t
      -> s
      -> [t]
alex' mo al t_err = loop
  where
    loop s = case lengthR s == 0 of
      True  -> []
      False -> choose al s

    choose []           _ = [t_err]
    choose ((re,f):al') s = case mb_p of
        Just (s',t) -> t : loop s'
        _           -> choose al' s
      where
        mb_p = do
          cap <- matchCapture mtch
          case captureOffset cap == 0 of
            True  -> (,) (captureSuffix cap) <$> f mtch
            False -> Nothing

        mtch = mo re s
\end{code}
