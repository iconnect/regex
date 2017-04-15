re-pp and re-include test file
==============================

This is a psuedo-Haskell script used to test the re-pp (and it cut-down
variant, re-include).


Here we have a couple of single-line vanilla code fragment.
\begin{code}
{-# LANGUAGE QuasiQuotes                      #-}
\end{code}

\begin{code}
\end{code}

And here is an empty one.
\begin{code}
\end{code}

The top main stuff:

A multi-line vanilla code fragment.

\begin{code}
import           Control.Applicative
\end{code}


An (self-)include directive
<div class='includedcodeblock'>
\begin{code}
evalme_PPT_01 = checkThis "" (Just 0) $ (length <$> Just [])
\end{code}
</div>


evalme frgment 1
\begin{code}
ghci> length []
0
\end{code}

evalme frgment 1
\begin{code}
ghci> length <$> Just []
Just 0
\end{code}

And the main bottom stuff.
