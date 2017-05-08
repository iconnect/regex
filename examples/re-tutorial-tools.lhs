The Regex Tools Tutorial
========================

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
import qualified Data.ByteString.Lazy.Char8               as LBS
import           Data.List
import           TestKit
import           Text.RE.Replace
import           Text.RE.TDFA.String
import           Text.RE.Tools
\end{code}


IsRegex, PCRE and TDFA
----------------------

The `IsRegex re tx` provides regex methods for the RE type `re` (belonging
to either the TDFA or PCRE back end) and a text type `tx` that the `re`
back end accepts. The `Text.RE.TDFA` and `Text.RE.PCRE` API modules provide
functions that work over all the text types, with the following match operators:

```haskell
(*=~)  :: IsRegex RE s
       => s
       -> RE
       -> Matches s

(?=~)  :: IsRegex RE s
       => s
       -> RE
       -> Match s

(*=~/) :: IsRegex RE s => s -> SearchReplace RE s -> s

(?=~/) :: IsRegex RE s => s -> SearchReplace RE s -> s
```


General IsRegex Functions
-------------------------

The `IsRegex` class is located in `Text.RE.Tools.IsRegex`:

%include "Text/RE/ZeInternals/Types/IsRegex.lhs" "^class Replace s => IsRegex re s"

Using these functions you can write your own regex tools. As a trivial example
we will define fully overloaded regex match operators as follows.
\begin{code}
(?=~%) :: IsRegex re s => s -> re -> Match s
(?=~%) = flip matchOnce

(*=~%) :: IsRegex re s => s -> re -> Matches s
(*=~%) = flip matchMany
\end{code}

\begin{code}
evalme_MYO_01 = checkThis "evalme_MYO_01" (True) $ matched $ (LBS.pack "2016-01-09 2015-12-5 2015-10-05") ?=~% [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
\end{code}
\begin{code}
evalme_MYO_02 = checkThis "evalme_MYO_02" (2) $ countMatches $ (LBS.pack "2016-01-09 2015-12-5 2015-10-05") *=~% [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
\end{code}

`regex` provides some classic tools that have quickly proven themselves
in the examples and scripts used to maintain regex itself.


The regex Tools
---------------

The classic tools assocciated with regular expressions have inspired some
regex conterparts.

  * [Text.RE.Tools.Grep](Grep.html): takes a regular expression and a
    file or lazy ByteString (depending upon the variant) and returns all of the
    matching lines.

  * [Text.RE.Tools.Lex](Lex.html): takes an association list of REs and
    token-generating functions and the input text and returns a list of tokens.
    This should never be used where performance is important (use Alex),
    except as a development prototype.

  * [Text.RE.Tools.Sed](Sed.html) using [Text.RE.Tools.Edit](Edit.html):
    takes an association list of regular expressions and substitution actions,
    some input text and invokes the associated action on each line of the file
    that matches one of the REs, substituting the text returned from the action
    in the output stream.

  * [Text.RE.Tools.Find](Find.html): scans a directory tree in the
    file system executing an action against all of the files that match
    RE.

These tools are built on top of the core library and act as good examples
of how to use the regex library as well as useful tools.

The following sections will present some of the internal library code
used to build the tools as well as some code from the example programs.
These fragments work best as starting points for studying these tools.


Sed and Edit
------------

`Edits` scripts are applied to each line of the text by the `sed`
functions.

%include "Text/RE/Tools/Edit.lhs" "data Edits"

`sed'` applies the script in its first argument to each line in the text
in its second argument.

%include "Text/RE/Tools/Sed.lhs" "sed' ::"

The `sed'` function is used to build the include processor in the
[`TestKit`](TestKit) utility modules used by the example scripts and
programs. To filter lines to exclude the `grepFilter` function is used.

%include "examples/TestKit.lhs" "include ::"


Grep
----

The `grepFilter` function takes an RE and a text and returns
the result of matching the RE to every line in the file.

%include "Text/RE/Tools/Grep.lhs" "grepFilter ::"

%include "Text/RE/Tools/Grep.lhs" "^data Line"

The `sortImports` utility in the [`TestKit`](TestKit) utility module
used by the scripts and example programs. It uses `grep` to sort all of
the imports by the name of the module in a single block located at the
position of the first import statement in the module, where each import
statement is in a standard form matched by the regex
```haskell
[re|^import +(qualified|         ) ${mod}([^ ].*)$|]
```

We have reproduced `sortImports` under the name `sortImports_` here.
\begin{code}
sortImports_ :: LBS.ByteString -> LBS.ByteString
sortImports_ lbs =
    LBS.unlines $ map (matchesSource . getLineMatches) $
      hdr ++ sortBy cMp bdy
  where
    cMp ln1 ln2 = case (extr ln1,extr ln2) of
        (Nothing,Nothing) -> EQ
        (Nothing,Just _ ) -> GT
        (Just _ ,Nothing) -> LT
        (Just x ,Just  y) -> compare x y

    extr ln = case allMatches $ getLineMatches ln of
      mtch:_  -> mtch !$$? [cp|mod|]
      _       -> Nothing

    (hdr,bdy) = span (not . anyMatches . getLineMatches) lns
    lns       = grepFilter rex lbs
    rex       = [re|^import +(qualified )? *${mod}([^ ].*)$|]
\end{code}

\begin{code}
evalme_GRP_01 = checkThisWith packLBS "evalme_GRP_01" ("-- preamble\nimport Data.List\nimport qualified Data.Text as T\n-- done\n") $ sortImports_ $ LBS.pack "-- preamble\nimport qualified Data.Text as T\nimport Data.List\n-- done\n"
\end{code}


Lex
---

The Lex toolkit can be used for quickly knocking together
scanners that do not need to be efficient.

%include "Text/RE/ZeInternals/Tools/Lex.lhs" "alex ::"

It has been used in the library to scan REs so that the captures can
be picked out, numbered and that number associated with a name where one
has been given.

%include "Text/RE/ZeInternals/NamedCaptures.lhs" "scan ::"


Find
----

The `findMatches_` function lists all of the files in a directort tree
that match an RE.

%include "Text/RE/Tools/Find.lhs" "findMatches_ ::"

It is used by the [`re-sort-imports`](re-sort-imports) program to discover
all of the Haskell scripts in the regex source tree and sort their import
statements into a standard order (ultimately using the
above-mentioned `sortImport` function).

%include "examples/re-sort-imports.lhs" "sort_r ::"


\begin{code}
main :: IO ()
main = runTheTests
  [ evalme_GRP_01
  , evalme_MYO_02
  , evalme_MYO_01
  ]
\end{code}

