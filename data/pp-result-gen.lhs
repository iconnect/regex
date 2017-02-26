re-pp and re-include test file
==============================

This is a psuedo-Haskell script used to test the re-pp (and it cut-down
variant, re-include).


Here we have a couple of single-line vanilla code fragment.
\begin{code}
{-# LANGUAGE QuasiQuotes                      #-}
\end{code}

\begin{code}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
\end{code}

And here is an empty one.
\begin{code}
\end{code}

The top main stuff:
\begin{code}
module Main(main) where
\end{code}

*********************************************************
*
* WARNING: this is generated from pp-tutorial-master.lhs 
*
*********************************************************


A multi-line vanilla code fragment.

\begin{code}
import           Control.Applicative
import           TestKit
\end{code}


An (self-)include directive
%include "data/pp-test.lhs" "^evalme_PPT_01"


A one-line evalme fragment.
\begin{code}
evalme_PPT_00 = checkThis "" 0 $
  length []
\end{code}

An evalme fragment spread over a couple of lines.
\begin{code}
evalme_PPT_01 = checkThis "evalme_PPT_01" (Just 0) $
  length <$>
    Just []
\end{code}

And the main bottom stuff.
\begin{code}
main :: IO ()
main = runTests
  [ evalme_PPT_01
  ]
\end{code}

