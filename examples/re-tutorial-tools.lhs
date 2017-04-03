The Regex Options Tutorial
==========================

\begin{code}
{-# LANGUAGE QuasiQuotes                      #-}
{-# LANGUAGE NoImplicitPrelude                #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
\end{code}


\begin{code}
import           Prelude.Compat
import           TestKit
import           Text.RE.TDFA.String
\end{code}


\begin{code}
evalme_TRD_00 = checkThis "evalme_TRD_00" (True) $ ("2016-01-09 2015-12-5 2015-10-05" =~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|] :: Bool)
\end{code}


\begin{code}
main :: IO ()
main = runTheTests
  [ evalme_TRD_00
  ]
\end{code}

