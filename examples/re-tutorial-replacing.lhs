The regex Replacing Tutorial
============================


Language Options and  Imports
-----------------------------

This tutorial is a literate Haskell program whwre we start by specifying
the language pragmas and imports we will need for this module.

\begin{code}
{-# LANGUAGE QuasiQuotes                      #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
\end{code}

\begin{code}
module Main(main) where
\end{code}

*********************************************************
*
* WARNING: this is generated from pp-tutorial-master.lhs 
*
*********************************************************


\begin{code}
import           TestKit
import           Text.RE.Replace
import           Text.RE.TDFA.String
\end{code}


Simple Text Replacement
-----------------------

regex supports the replacement of matched text with alternative text. This
section will cover replacement text specified with templates. More flexible
tools that allow functions calculate the replacement text are covered below.

_Capture_ sub-expressions, whose matched text can be inserted into the
replacement template, can be specified as follows:

  * `$(` ... `)` identifies a capture that can be identified by its
    left-to-right position relative to the other captures in the replacement
    template, with `$1` being used to represent the leftmost capture, `$2` the
    next leftmost capture, and so on;

  * `${foo}(` ... `)` can be used to identify a capture by name. Such captures
    can be identified either by their left-to-right position in the regular
    expression or by `${foo}` in the template.

A function to convert ISO format dates into a UK-format date could be written
thus:
\begin{code}
uk_dates :: String -> String
uk_dates src =
  replaceAll "${d}/${m}/${y}" $ src *=~ [re|${y}([0-9]{4})-${m}([0-9]{2})-${d}([0-9]{2})|]
\end{code}
with
\begin{code}
evalme_RPL_01 = checkThis "evalme_RPL_01" ("09/01/2016 2015-12-5 05/10/2015") $ uk_dates "2016-01-09 2015-12-5 2015-10-05"
\end{code}

The same function written with numbered captures:
\begin{code}
uk_dates' :: String -> String
uk_dates' src =
  replaceAll "$3/$2/$1" $ src *=~ [re|$([0-9]{4})-$([0-9]{2})-$([0-9]{2})|]
\end{code}
with
\begin{code}
evalme_RPL_02 = checkThis "evalme_RPL_02" ("09/01/2016 2015-12-5 05/10/2015") $ uk_dates' "2016-01-09 2015-12-5 2015-10-05"
\end{code}
yielding the same result.

(Most regex conventions use plain parentheses, `(` ... `)`, to mark
captures but we would like to reserve those exclusively for grouping
in regex REs.)


Matches/Match/Capture
---------------------

The types returned by the `?=~` and `*=~` form the foundations of the
package. Understandingv these simple types is the key to understanding
the package.

The type of `*=~` in this module (imported from
`Text.RE.TDFA.String`) is:
<div class='inlinecodeblock'>
```haskell
(*=~) :: String -> RE -> Matches String
```
</div>
with `Matches` defined in `Text.RE.ZeInternals.Types.Capture` thus:

%include "Text/RE/ZeInternals/Types/Matches.lhs" "^data Matches "

The critical component of the `Matches` type is the `[Match a]` in
`allMatches`, containing the details all of each substring matched by
the RE. The `matchSource` component also retains a copy of the original
search string but the critical information is in `allmatches`.

The type of `?=~` in this module (imported from
`Text.RE.TDFA.String`) is:
<div class='inlinecodeblock'>
```haskell
(?=~) :: String -> RE -> Match String
```
</div>
with `Match` (referenced in the definition of `Matches` above) defined
in `Text.RE.ZeInternals.Types.Capture` thus:

%include "Text/RE/ZeInternals/Types/Match.lhs" "^data Match "

Like `matchesSource` above, `matchSource` retains the original search
string, but also a `CaptureNames` field listing all of the capture
names in the RE (needed by the text replacemnt tools).

But the 'real' content of `Match` is to be found in the `MatchArray`,
enumerating all of the substrings captured by this match, starting with
`0` for the substring captured by the whole RE, `1` for the leftmost
explicit capture in the RE, `2` for the next leftmost capture, and so
on.

Each captured substring is represented by the following `Capture` type:

%include "Text/RE/ZeInternals/Types/Capture.lhs" "^data Capture "

Here we list the whole original search string in `captureSource` and
the text of the sub-string captured in `capturedText`. `captureOffset`
contains the number of characters preceding the captured substring, or
is negative if no substring was captured (which is a different
situation from epsilon, the empty string, being captured).
`captureLength` gives the length of the captured string in
`capturedText`.

The test suite in [examples/re-tests.lhs](re-tests.html) contains extensive
worked-out examples of these `Matches`/`Match`/`Capture` types.


Using Functions to Replace Text
-------------------------------

Sometimes you will need to process each string captured by an RE with a
function. `replaceAllCaptures` takes a `REContext`, a substitution
function and a `Matches` and applies the function to each captured
substring according to the `REContext`, as we can see in the following
example function to clean up all of the mis-formatted dates in the
argument string,
\begin{code}
fixup_dates :: String -> String
fixup_dates src =
    replaceAllCaptures SUB phi $ src *=~ [re|([0-9]+)-([0-9]+)-([0-9]+)|]
  where
    phi _ loc cap = Just $ case locationCapture loc of
        1 -> fmt 4 $ read s
        2 -> fmt 2 $ read s
        3 -> fmt 2 $ read s
        _ -> error "fixup_dates"
      where
        s = capturedText cap

fmt :: Int -> Int -> String
fmt w x = replicate (max 0 $ w - length x_s ) '0' ++ x_s
  where
    x_s = show x

\end{code}
which will fix up our running example
\begin{code}
evalme_RPF_01 = checkThis "evalme_RPF_01" ("2016-01-09 2015-12-05 2015-10-05") $ fixup_dates "2016-01-09 2015-12-5 2015-10-05"
\end{code}

The `replaceAllCaptures` function is of type

%include "Text/RE/ZeInternals/Replace.lhs" "replaceAllCaptures ::"

and the `REContext` and `RELocation` types are defined in
`Text.RE.Replace` as follows,

%include "Text/RE/ZeInternals/Replace.lhs" "^data REContext"

The processing function gets applied to the captures specified by the
`REContext`, which can be directed to process `ALL` of the captures,
including the substring captured by the whole RE and all of the
subsidiary capture, or just the `TOP`, `0` capture that the whole RE
matches, or just the `SUB` (subsidiary) captures, as was the case above.

The substitution function takes the `Match` corresponding to the current
redex being processed, the `RELocation` information specifying redex _n_
redex and capure _i_, and the `Capure` being substituted. Our substitution
function didn't need the `Match` context so it ignored it.

The substition function either return `Nothing` to indicate that no
substitution should be made or the replacement text.

The above fixup function could be extended to enclose whole date in
square brackets by specifying an `ALL` context and a `0` case for the
substitution function.
\begin{code}
fixup_and_reformat_dates :: String -> String
fixup_and_reformat_dates src =
    replaceAllCaptures ALL f $ src *=~ [re|([0-9]+)-([0-9]+)-([0-9]+)|]
  where
    f _ loc cap = Just $ case locationCapture loc of
        0 -> "["++txt++"]"
        1 -> fmt 4 $ read txt
        2 -> fmt 2 $ read txt
        3 -> fmt 2 $ read txt
        _ -> error "fixup_date"
      where
        txt = capturedText cap
\end{code}
The `fixup_and_reformat_dates` applied to our running example,
\begin{code}
evalme_RPF_02 = checkThis "evalme_RPF_02" ("[2016-01-09] [2015-12-05] [2015-10-05]") $ fixup_and_reformat_dates "2016-01-09 2015-12-5 2015-10-05"
\end{code}

`Text.RE.Replace` provides analagous functions for replacing the
test of a single `Match` returned from `?=~`.


\begin{code}
main :: IO ()
main = runTheTests
  [ evalme_RPF_02
  , evalme_RPF_01
  , evalme_RPL_02
  , evalme_RPL_01
  ]
\end{code}

