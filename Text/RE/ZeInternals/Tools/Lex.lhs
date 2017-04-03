\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}

module Text.RE.ZeInternals.Tools.Lex
  ( alex
  , alex'
  ) where

import           Prelude.Compat
import           Text.RE.Replace
import           Text.RE.ZeInternals.Types.IsRegex


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
