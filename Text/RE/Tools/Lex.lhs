\begin{code}
{-# LANGUAGE NoImplicitPrelude          #-}

module Text.RE.Tools.Lex where

import           Prelude.Compat
import           Text.RE.Types.Capture
import           Text.RE.Types.IsRegex
import           Text.RE.Replace


alex :: IsRegex re s => [(re,Match s->Maybe t)] -> t -> s -> [t]
alex = alex' matchOnce

alex' :: Replace s
      => (re->s->Match s)
      -> [(re,Match s->Maybe t)]
      -> t
      -> s
      -> [t]
alex' mo al t_err = loop
  where
    loop s = case lengthE s == 0 of
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
