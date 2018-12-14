The regex Tutorial
==================

This tutorial is a self-testing literate Haskell programme introducing
the vanilla API of the [regex package](http://hs.regex.uk). There
are other tutorials for explaining the more specialist aspects of regex
and you can load them into into you Haskell REPL of choice: see the
[regex Tutorials page](http://tutorial.regex.uk) for details.


Language Pragmas
----------------

The first thing you will have to do is enable `QuasiQuotes` as regex
uses them to check that REs are well-formed at compile time.
\begin{code}
{-# LANGUAGE QuasiQuotes                      #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
\end{code}
If you are trying out examples interactively at the ghci prompt then you
will need
```
:seti -XQuasiQuotes
```

Importing the API
-----------------

\begin{code}
module Main(main) where
\end{code}

*********************************************************
*
* WARNING: this is generated from pp-tutorial-master.lhs 
*
*********************************************************


Before importing the `regex` API into your Haskell script you will need
to answer two questions:

  1. Which flavour of REs do I need? If you need Posix REs then the `TDFA`
     is for you, otherwise it is the PCRE back end, which is housed in
     a seperate `regex-with-pcre` package.

The import statement will in general look like this
```
  import Text.RE.<back-end>.<text-type>
```

As we have no interest in Posix/PCRE distinctions or performance here,
we have chosen to work with the `TDFA` back end with `String` types.

\begin{code}
import TestKit
import Text.RE.TDFA.String
\end{code}

You could also import `Text.RE.TDFA` or `Text.RE.PCRE` to get an API
in which the operators are overloaded over all text types accepted by
each of these back ends: see the [Tools Tutorial](re-tutorial-tools.html)
for details.


Single `Match` with `?=~`
-------------------------

The regex API provides two matching operators: one for looking for the first
match in its search string and the other for finding all of the matches. The
first-match operator, `?=~`, yields the result of attempting to find the first
match.
```
(?=~) :: String -> RE -> Match String
```
The boolean `matched` function,
```
matched :: Match a -> Bool
```
can be used to test whether a match was found:
\begin{code}
evalme_SGL_01 = checkThis "evalme_SGL_01" (True) $ matched $ "2016-01-09 2015-12-5 2015-10-05" ?=~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
\end{code}

To get the matched text use `matchText`,
```
matchedText :: Match a -> Maybe a
```
which returns `Nothing` if no match was found in the search string:
\begin{code}
evalme_SGL_02 = checkThis "evalme_SGL_02" (Just "2016-01-09") $ matchedText $ "2016-01-09 2015-12-5 2015-10-05" ?=~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
\end{code}
\begin{code}
evalme_SGL_03 = checkThis "evalme_SGL_03" (Nothing) $ matchedText $ "2015-12-5" ?=~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
\end{code}


Multiple `Matches` with `*=~`
-----------------------------

Use `*=~` to locate all of the non-overlapping substrings that match a RE,
```
(*=~)      :: String -> RE -> Matches String
anyMatches :: Matches a -> Bool
```
`anyMatches` can be used to determine if any matches were found
\begin{code}
evalme_MLT_01 = checkThis "evalme_MLT_01" (True) $ anyMatches $ "2016-01-09 2015-12-5 2015-10-05" *=~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
\end{code}
and `countMatches` will tell us how many sub-strings matched:
\begin{code}
evalme_MLT_02 = checkThis "evalme_MLT_02" (2) $ countMatches $ "2016-01-09 2015-12-5 2015-10-05" *=~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
\end{code}
`matches` will return all of the matches.
```
matches :: Natches a -> [a]
```
\begin{code}
evalme_MLT_03 = checkThis "evalme_MLT_03" (["2016-01-09","2015-10-05"]) $ matches $ "2016-01-09 2015-12-5 2015-10-05" *=~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
\end{code}


The `regex` Macros and Parsers
------------------------------

regex supports macros in regular expressions. There are a bunch of
standard macros that you can just use, and you can define your own.

RE macros are enclosed in `@{` ... '}'. By convention the macros in
the standard environment start with a '%'. `@{%date}` will match an
ISO 8601 date, this
\begin{code}
evalme_MAC_00 = checkThis "evalme_MAC_00" (2) $ countMatches $ "2016-01-09 2015-12-5 2015-10-05" *=~ [re|@{%date}|]
\end{code}
will pick out the two dates.

There are also parsing functions for analysing the matched text. The
`@{%string}` macro will match quoted strings (in which double quotes can be
escaped with backslashes in the usual way) and its companion `parseString`
function will extract the string that was being quoted, interpreting any
escaped double quotes:
\begin{code}
evalme_MAC_01 = checkThisWith convertMaybeTextList "evalme_MAC_01" ([Just "foo",Just "bar", Just "\""]) $ map parseString $ matches $ "\"foo\", \"bar\" and a quote \"\\\"\"" *=~ [re|@{%string}|]
\end{code}

See the [macro tables page](http://macros.regex.uk) for details of the standard macros and their parsers.

See the [testbench tutorial](re-tutorial-testbench.html) for more on how
you can develop, document and test RE macros with the regex test bench.


Search and Replace
------------------

If you need to edit a string then `SearchReplace` `[ed|` ... `|]`
templates can be used with `?=~/` to replace a single instance or
`*=~/` to replace all matching instances.

\begin{code}
evalme_SRP_00 = checkThis "evalme_SRP_00" ("0x0000: 40AA fab0") $ "0000 40AA fab0" ?=~/ [ed|${adr}([0-9A-Fa-f]{4}):?///0x${adr}:|]
\end{code}
\begin{code}
evalme_SRP_01 = checkThis "evalme_SRP_01" ("0x0000: 0x40AA 0xfab0") $ "0000: 40AA fab0" *=~/ [ed|[0-9A-Fa-f]{4}///0x$0|]
\end{code}


Specifying Options
------------------

By default regular expressions are of the multi-line case-sensitive
variety so this
\begin{code}
evalme_SOP_01 = checkThis "evalme_SOP_01" (2) $ countMatches $ "0a\nbb\nFe\nA5" *=~ [re|[0-9a-f]{2}$|]
\end{code}
will find 2 matches, the '$' anchor matching each of the newlines, but only
the first two lowercase hex numbers matching the RE. The case sensitivity
and multiline-ness can be controled by selecting alternative parsers.

+--------------------------+-------------+-----------+----------------+
| long name                | short forms | multiline | case sensitive |
+==========================+=============+===========+================+
| reMultilineSensitive     | reMS, re    | yes       | yes            |
+--------------------------+-------------+-----------+----------------+
| reMultilineInsensitive   | reMI        | yes       | no             |
+--------------------------+-------------+-----------+----------------+
| reBlockSensitive         | reBS        | no        | yes            |
+--------------------------+-------------+-----------+----------------+
| reBlockInsensitive       | reBI        | no        | no             |
+--------------------------+-------------+-----------+----------------+

So while the default setup
\begin{code}
evalme_SOP_02 = checkThis "evalme_SOP_02" (2) $ countMatches $ "0a\nbb\nFe\nA5" *=~ [reMultilineSensitive|[0-9a-f]{2}$|]
\end{code}
finds 2 matches, a case-insensitive RE
\begin{code}
evalme_SOP_03 = checkThis "evalme_SOP_03" (4) $ countMatches $ "0a\nbb\nFe\nA5" *=~ [reMultilineInsensitive|[0-9a-f]{2}$|]
\end{code}
finds 4 matches, while a non-multiline RE
\begin{code}
evalme_SOP_04 = checkThis "evalme_SOP_04" (0) $ countMatches $ "0a\nbb\nFe\nA5" *=~ [reBlockSensitive|[0-9a-f]{2}$|]
\end{code}
finds no matches but a non-multiline, case-insensitive match
\begin{code}
evalme_SOP_05 = checkThis "evalme_SOP_05" (1) $ countMatches $ "0a\nbb\nFe\nA5" *=~ [reBlockInsensitive|[0-9a-f]{2}$|]
\end{code}
finds the final match.

For the hard of typing the shortforms are available.
\begin{code}
evalme_SOP_06 = checkThis "evalme_SOP_06" (True) $ matched $ "SuperCaliFragilisticExpialidocious" ?=~ [reMI|supercalifragilisticexpialidocious|]
\end{code}


Compiling and Escaping
----------------------

It is possible to compile a dynamically aquired RE string at run-time using
`compileRegex`:
```
compileRegex :: (Functor m, Monad m) => String -> m RE
```
\begin{code}
evalme_CPL_01 = checkThis "evalme_CPL_01" (["2016-01-09","2015-10-05"]) $ matches $ "2016-01-09 2015-12-5 2015-10-05" *=~ (either error id $ compileRegex "[0-9]{4}-[0-9]{2}-[0-9]{2}")
\end{code}

These will compile the RE using the default multiline, case-sensitive options,
but you can specify the options dynamically using `compileRegexWith`:
```
compileRegexWith :: (Functor m, Monad m) => SimpleREOptions -> String -> m RE
```
where `SimpleREOptions` is a simple enumerated type.

%include "Text/RE/REOptions.lhs" "^data SimpleREOptions"

\begin{code}
evalme_CPL_02 = checkThis "evalme_CPL_02" (["2016-01-09","2015-10-05"]) $ matches $ "2016-01-09 2015-12-5 2015-10-05" *=~ (either error id $ compileRegexWith MultilineSensitive "[0-9]{4}-[0-9]{2}-[0-9]{2}")
\end{code}

If you need to compile `SearchReplace` templates for use with `?=~/` and
`*=~/` then the `compileSearchReplace` and `compileSearchReplaceWith`,
```
compileSearchReplace     :: (Monad m, Functor m, IsRegex RE s) => String -> String -> m (SearchReplace RE s)
compileSearchReplaceWith :: (Monad m, Functor m, IsRegex RE s) => SimpleREOptions -> String -> String -> m (SearchReplace RE s)
```
work analagously to `compileRegex` and `compileRegexWith`, with the RE
and replacement template (either side of the '///' in the `[ed|...///...|]`
quasi quoters) being passed into these functions in two separate strings,
to compile to the `SearchReplace` type expected by the `?=~/` and `*=~/`
operators.

%include "Text/RE/ZeInternals/Types/SearchReplace.lhs" "^data SearchReplace"

The `escape` and `escapeWith` functions are special compilers that compile
a string into a RE that should match itself, which is assumed to be embedded
in a complex RE to be compiled.
```
escape :: (Functor m, Monad m) => (String->String) -> String -> m RE
```
The function pased in the first argument to `escape` takes the RE string
that will match the string passed in the second argument and yields the
RE to be compiled, which is returned from the parsing action.
\begin{code}
evalme_CPL_03 = checkThis "evalme_CPL_03" ("foobar") $ "fooe{0}bar" *=~/ SearchReplace (either error id $ escape id "e{0}") ""
\end{code}


The Classic regex-base Match Operators
--------------------------------------

The original `=~` and `=~~` match operators are still available for
those that have mastered them.
\begin{code}
evalme_CLC_01 = checkThis "evalme_CLC_01" (True )    $ ("bar"    =~  [re|(foo|bar)|] :: Bool)
\end{code}
\begin{code}
evalme_CLC_02 = checkThis "evalme_CLC_02" (False)    $ ("quux"   =~  [re|(foo|bar)|] :: Bool)
\end{code}
\begin{code}
evalme_CLC_03 = checkThis "evalme_CLC_03" (2)        $ ("foobar" =~  [re|(foo|bar)|] :: Int)
\end{code}
\begin{code}
evalme_CLC_04 = checkThis "evalme_CLC_04" (Nothing)  $ ("foo"    =~~ [re|bar|]       :: Maybe String)
\end{code}

\begin{code}
main :: IO ()
main = runTheTests
  [ evalme_CLC_04
  , evalme_CLC_03
  , evalme_CLC_02
  , evalme_CLC_01
  , evalme_CPL_03
  , evalme_CPL_02
  , evalme_CPL_01
  , evalme_SOP_06
  , evalme_SOP_05
  , evalme_SOP_04
  , evalme_SOP_03
  , evalme_SOP_02
  , evalme_SOP_01
  , evalme_SRP_01
  , evalme_SRP_00
  , evalme_MAC_01
  , evalme_MAC_00
  , evalme_MLT_03
  , evalme_MLT_02
  , evalme_MLT_01
  , evalme_SGL_03
  , evalme_SGL_02
  , evalme_SGL_01
  ]
\end{code}

