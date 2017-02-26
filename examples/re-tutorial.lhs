The Regex Tutorial
==================

This is a literate Haskell programme lightly processed to produce this
web presentation and also to generate a test suite that verifies that
each of the example calculations are generating the expected results.
You can load it into ghci and try out the examples either by running
_ghci_ itself from the root folder of the regex package:
```bash
ghci examples/re-tutorial.lhs
```
or using `cabal repl`:
```
cabal configure --enable-tests
cabal repl examples/re-tutorial.lhs
```

Depending upon how you have configured and run `ghci` you may need to
set one of _ghci_'s interctive settings &mdash; the topic of the next
section.


Setting Up: The Pragmas
-----------------------

Haskell programs typically start with a few compiler pragmas to switch
on the language extensions needed by the module. Because regex uses
Template Haskell to check regular expressions at compile time `QuasiQuotes`
should be enabled.

\begin{code}
{-# LANGUAGE QuasiQuotes #-}
\end{code}

Use this command to configure ghci accordingly (not necessary if you
have launched ghci with `cabal repl`):
```
:seti -XQuasiQuotes
```

Because we are mimicking the REPL in this tutorial we will leave off the type
signatures on the example calculations and disable the compiler
warnings about missing type signatures.
\begin{code}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
\end{code}

This pragma is a just technical pragma, combined with the
`Prelude.Compat` import below used to avoid certain warnings while
comiling against multiple versions of the compiler. It can be safely
ignored.
\begin{code}
{-# LANGUAGE NoImplicitPrelude #-}
\end{code}


Loading Up: The Imports
-----------------------

\begin{code}
module Main(main) where
\end{code}

*********************************************************
*
* WARNING: this is generated from pp-tutorial-master.lhs 
*
*********************************************************

We have two things to consider in the import to access the regex
goodies, which will take the form:
```haskell
import Text.RE.<regex-flavour>.<text-type>?
```

  * Which [flavour of regular expressions](https://wiki.haskell.org/Regular_expressions) do I need?
      + `PCRE` : [Perl-style](https://en.wikibooks.org/wiki/Regular_Expressions/Perl-Compatible_Regular_Expressions) regular expressions;
      + `TDFA`: [Posix-style](https://en.wikipedia.org/wiki/Regular_expression#POSIX_extended) regular expressions.

  * And which type of text am I matching?
      + `String` : low-performing, classic Haskell strings;
      + `ByteString` : raw bytestrings;
      + `ByteString.Lazy` : raw lazy bytestrings;
      + `Text`: efficient text (currently available for `TDFA` only);
      + `Text.Lazy` : efficient, lazy text (currently available for
        `TDFA` only);
      + polymorphic: if you need matching operators that work with all
        available text types then do not specify the `<text-type>` in
        the import path.

For this tutorial we will use classic Haskell strings but any application
dealing with bulk text will probably want to choose one of the other
options.
\begin{code}
import           Text.RE.TDFA.String
\end{code}
If you are predominantly matching against a single type in your module
then you will probably find it more convenient to use the relevant module
rather than than the more polymorphic opertors but it is really a
matter of convenience.

We will also need access to a small selection of common libraries for
our examples.
\begin{code}
import           Control.Applicative
import           Data.Maybe
import qualified Data.Text                      as T
import           Text.Printf
\end{code}

And finally we a special edition of the prelude (see the commentary for
the pragma section above) and a small specail toolkit will be used to help
manage the example calculations.
\begin{code}
import           Prelude.Compat
import           TestKit
\end{code}
This allows simple calculations to be defined stylistically
in the source program, presented as calculations when rendered
in HTML and tested that they have the expected result.
\begin{code}
evalme_LOA_00 = checkThis "evalme_LOA_00" (0) $ length []
\end{code}
This trivial example calculation will be tested for zero-ness.


Matching with the regex-base Operators
--------------------------------------

regex supports the regex-base polymorphic match operators. Used in a
`Bool` context `=~` will evaluate to True iff the string on the left matches
the RE on the right.
\begin{code}
evalme_TRD_00 = checkThis "evalme_TRD_00" (True) $ ("2016-01-09 2015-12-5 2015-10-05" =~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|] :: Bool)
\end{code}
Note that we enclose the RE itself in `[re|` ... `|]` quasi quote brackets,
allowing the compiler to run some regex code at compile time to verify that
the RE conforms to the correct syntax for the chosen RE flavour of choice
(`TDFA` in this case). The above expression should evaluate to `True` as the
string contains a matching sub-string.

Used in an `Int` context `=~` will count the number of matches in the target string.
\begin{code}
evalme_TRD_01 = checkThis "evalme_TRD_01" (2) $ ("2016-01-09 2015-12-5 2015-10-05" =~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|] :: Int)
\end{code}

To determine the string that has matched the modaic `=~~` operator can be used
in a `Maybe` context.
\begin{code}
evalme_TRD_02 = checkThis "evalme_TRD_02" (Just "2016-01-09") $ ("2016-01-09 2015-12-5 2015-10-05" =~~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|] :: Maybe String)
\end{code}

A `=~` in a `[[String]]` extracts all of the matched substrings:
\begin{code}
evalme_TRD_04 = checkThis "evalme_TRD_04" ([["2016-01-09"],["2015-10-05"]]) $ ("2016-01-09 2015-12-5 2015-10-05" =~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|] :: [[String]])
\end{code}

regex provides special operators and types for extracting the first
match or all of the non-overlapping substrings matching a regular expression
which provide a little more structure that the flexible, venerable regex-base
match operators.


Single Matches with `?=~`
-------------------------

regex also provides two matching operators: one for looking for the first
match in its search string and the other for finding all of the matches. The
first-match operator, `?=~`, yields the result of attempting to find the first
match. (It's result type will be explained below.) The boolean `matched`
function can be used to test whether a match was found.
\begin{code}
evalme_SGL_01 = checkThis "evalme_SGL_01" (True) $ matched $ "2016-01-09 2015-12-5 2015-10-05" ?=~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
\end{code}

To get the matched text use `matchText`, which returns `Nothing` if no match was
found in the search string.
\begin{code}
evalme_SGL_02 = checkThis "evalme_SGL_02" (Just "2016-01-09") $ matchedText $ "2016-01-09 2015-12-5 2015-10-05" ?=~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
\end{code}


Multiple Matches with `*=~`
---------------------------

Use `*=~` to locate all of the non-overlapping substrings that matches a RE.
`anyMatches` will return `True` iff any matches are found (and they will be).
\begin{code}
evalme_MLT_01 = checkThis "evalme_MLT_01" (True) $ anyMatches $ "2016-01-09 2015-12-5 2015-10-05" *=~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
\end{code}

`countMatches` will tell us how many sub-strings matched (2).
\begin{code}
evalme_MLT_02 = checkThis "evalme_MLT_02" (2) $ countMatches $ "2016-01-09 2015-12-5 2015-10-05" *=~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
\end{code}

`matches` will return all of the matches.
\begin{code}
evalme_MLT_03 = checkThis "evalme_MLT_03" (["2016-01-09","2015-10-05"]) $ matches $ "2016-01-09 2015-12-5 2015-10-05" *=~ [re|[0-9]{4}-[0-9]{2}-[0-9]{2}|]
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
```
(*=~) :: String -> RE -> Matches String
```
</div>
with `Matches` defined in `Text.RE.Capture` thus:

%include "Text/RE/Capture.lhs" "^data Matches "

The critical component of the `Matches` type is the `[Match a]` in
`allMatches`, containing the details all of each substring matched by
the RE. The `matchSource` component also retains a copy of the original
search string but the critical information is in `allmatches`.

The type of `?=~` in this module (imported from
`Text.RE.TDFA.String`) is:
<div class='inlinecodeblock'>
```
(?=~) :: String -> RE -> Match String
```
</div>
with `Match` (referenced in the definition of `Matches` above) defined
in `Text.RE.Capture` thus:

%include "Text/RE/Capture.lhs" "^data Match "

Like `matchesSource` above, `matchSource` retains the original search
string, but also a `CaptureNames` field listing all of the capture
names in the RE (needed by the text replacemnt tools).

But the 'real' content of `Match` is to be found in the `MatchArray`,
enumerating all of the substrings captured by this match, starting with
`0` for the substring captured by the whole RE, `1` for the leftmost
explicit capture in the RE, `2` for the next leftmost capture, and so
on.

Each captured substring is represented by the following `Capture` type:

%include "Text/RE/Capture.lhs" "^data Capture "

Here we list the whole original search string in `captureSource` and
the text of the sub-string captured in `capturedText`. `captureOffset`
contains the number of characters preceding the captured substring, or
is negative if no substring was captured (which is a different
situation from epsilon, the empty string, being captured).
`captureLength` gives the length of the captured string in
`capturedText`.

The test suite in [examples/re-tests.lhs](re-tests.html) contains extensive
worked-out examples of these `Matches`/`Match`/`Capture` types.


Simple Options
--------------

By default regular expressions are of the multi-line case-sensitive
variety so this query
\begin{code}
evalme_SOP_01 = checkThis "evalme_SOP_01" (2) $ countMatches $ "0a\nbb\nFe\nA5" *=~ [re|[0-9a-f]{2}$|]
\end{code}
finds 2 matches, the '$' anchor matching each of the newlines, but only
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


Using Functions to Replace Text
-------------------------------

Sometimes you will need to process each string captured by an RE with a
function. `replaceAllCaptures` takes a `Context`, a substitution
function and a `Matches` and applies the function to each captured
substring according to the `Context`, as we can see in the following
example function to clean up all of the mis-formatted dates in the
argument string,
\begin{code}
fixup_dates :: String -> String
fixup_dates src =
    replaceAllCaptures SUB phi $ src *=~ [re|([0-9]+)-([0-9]+)-([0-9]+)|]
  where
    phi _ loc cap = Just $ case locationCapture loc of
        1 -> printf "%04d" (read s :: Int)
        2 -> printf "%02d" (read s :: Int)
        3 -> printf "%02d" (read s :: Int)
        _ -> error "fixup_dates"
      where
        s = capturedText cap
\end{code}
which will fix up our running example
\begin{code}
evalme_RPF_01 = checkThis "evalme_RPF_01" ("2016-01-09 2015-12-05 2015-10-05") $ fixup_dates "2016-01-09 2015-12-5 2015-10-05"
\end{code}

The `replaceAllCaptures` function is of type

%include "Text/RE/Replace.lhs" "replaceAllCaptures ::"

and the `Context` and `Location` types are defined in
`Text.RE.Replace` as follows,

%include "Text/RE/Replace.lhs" "^data Context"

The processing function gets applied to the captures specified by the
`Context`, which can be directed to process `ALL` of the captures,
including the substring captured by the whole RE and all of the
subsidiary capture, or just the `TOP`, `0` capture that the whole RE
matches, or just the `SUB` (subsidiary) captures, as was the case above.

The substitution function takes the `Match` corresponding to the current
redex being processed, the `Location` information specifying redex _n_
redex and capure _i_, and the `Capure` being substituted. Our substitution
function didn't need the `Match` context so it ignored it.

The substition function either return `Nothing` to indicate that no
substitution should be made or the replacement text.

The above fixup function could be extended to enclose whole date in
square brackets by specifing an `ALL` context and a `0` case for the
substitution function.
\begin{code}
fixup_and_reformat_dates :: String -> String
fixup_and_reformat_dates src =
    replaceAllCaptures ALL f $ src *=~ [re|([0-9]+)-([0-9]+)-([0-9]+)|]
  where
    f _ loc cap = Just $ case locationCapture loc of
        0 -> printf "[%s]"       txt
        1 -> printf "%04d" (read txt :: Int)
        2 -> printf "%02d" (read txt :: Int)
        3 -> printf "%02d" (read txt :: Int)
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


Macros and Parsers
------------------

regex supports macros in regular expressions. There are a bunch of
standard macros and you can define your own.

RE macros are enclosed in `@{` ... '}'. By convention the macros in
the standard environment start with a '%'. `@{%date}` will match an
ISO 8601 date, this
\begin{code}
evalme_MAC_00 = checkThis "evalme_MAC_00" (2) $ countMatches $ "2016-01-09 2015-12-5 2015-10-05" *=~ [re|@{%date}|]
\end{code}
picking out the two dates.

See the tables listing the standard macros in the tables folder of
the distribution.

See the log-processor example and the `Text.RE.TestBench` for
more on how you can develop, document and test RE macros with the
regex test bench.


Compiling REs with the Complete Options
---------------------------------------

Each type of RE &mdash; TDFA and PCRE &mdash; has it own complile-time
options and execution-time options, called in each case `CompOption` and
`ExecOption`. The above simple options selected with the RE
parser (`reMultilineSensitive`, etc.) configures the RE backend
accordingly so that you don't have to, but you may need full access to
you chosen back end's options, or you may need to supply a different
set of macros to those provided in the standard environment. In which
case you will need to know about the `Options` type, defined by each of
the back ends in terms of the `Options_` type of `Text.RE.Options`
as follows.
<div class='inlinecodeblock'>
```
type Options = Options_ RE CompOption ExecOption
```
</div>
(Bear in mind that `CompOption` and `ExecOption` will be different
types for each back end.)

The `Options_` type is defined in `Text.RE.Options` as follows:

%include "Text/RE/Options.lhs" "data Options_"

  * `optionsMode` is an experimental feature that controls the RE
    parser.

  * `optionsMacs` contains the macro definitions used to compile
    the REs (see above Macros section);

  * `optionsComp` contains the back end compile-time options;

  * `optionsExec` contains the back end execution-time options.

(For more information on the options provided by the back ends see the
decumentation for `regex-tdfa` and `regex-pcre` as apropriate.)

Each backend provides a function to compile REs from some options and a
string containing the RE as follows:
<div class='inlinecodeblock'>
```
compileRegex :: ( IsOption o RE CompOption ExecOption
                , Functor m
                , Monad   m
                )
             => o
             -> String
             -> m RE
```
</div>
where `o` is some type that is recognised as a type that can configure
REs. Your configuration-type options are:

  * `()` (the unit type) means just use the default multi-line
    case-sensitive that we get with the `re` parser.

  * `SimpleRegexOptions` this is just a simple enum type that we use to
    encode the standard options:

%include "Text/RE/Options.lhs" "^data SimpleRegexOptions"

  * `Mode`: you can specify the parser mode;

  * `Macros RE`: you can specify the macros use instead of the standard
    environment;

  * `CompOption`: you can specify the compile-time options for the back
    end;

  * `ExecOption`: you can specify the execution-time options for the
    back end;

  * `Options`: you can specify all of the options.

The compilation may fail so it is expressed monadically. For the
following examples we will use the following helper to just `error`
the failure.
\begin{code}
check_for_failure :: Either String a -> a
check_for_failure = either error id
\end{code}

\begin{code}
evalme_OPT_00 = checkThis "evalme_OPT_00" (2) $ countMatches $ "2016-01-09 2015-12-5 2015-10-05" *=~ check_for_failure (compileRegex () "@{%date}")
\end{code}
\begin{code}
evalme_OPT_01 = checkThis "evalme_OPT_01" (1) $ countMatches $ "0a\nbb\nFe\nA5" *=~ check_for_failure (compileRegex BlockInsensitive "[0-9a-f]{2}$")
\end{code}

This will allow you to compile regular expressions when the either the
text to be compiled or the options have been dynamically determined.


Specifying Options with `re_`
-----------------------------

If you just need to specify some non-standard options while statically
checking the validity of the RE (with the default options) then you can
use the `re_` parser:
\begin{code}
evalme_REU_01 = checkThis "evalme_REU_01" (1) $ countMatches $ "0a\nbb\nFe\nA5" *=~ [re_|[0-9a-f]{2}$|] BlockInsensitive
\end{code}
Any option `o` such that `IsOption o RE CompOption ExecOption` (i.e.,
any option type accepted by `compileRegex` above) can be used with
`[re_` ..`|]`.


The Tools: 'grep', 'lex' and 'sed'
----------------------------------

The classic tools assocciated with regular expressions have inspired some
regex conterparts.

  * [Text.RE.Tools.Grep}(Grep.html): takes a regular expression and a
    file or lazy ByteString (depending upon the variant) and returns all of the
    matching lines. (Used in the [include](re-include.html) example.)

  * [Text.RE.Tools.Lex}(Lex.html): takes an association list of REs and
    token-generating functions and the input text and returns a list of tokens.
    This should never be used where performance is important (use Alex),
    except as a development prototype (used internally in
    [Text.RE.Internal.NamedCaptures](NamedCaptures.html)).

  * [Text.RE.Tools.Lex}(Sed.html) using [Text.RE.Edit](Edit.html):
    takes an association list of regular expressions and substitution actions,
    some input text and invokes the associated action on each line of the file
    that matches one of the REs, substituting the text returned from the action
    in the output stream. (Used in the [include](re-include.html),
    [gen-modules](re-gen-modules.html),
    [log-processor](re-nginx-log-processor.html) and [tutorial-pp](re-prep)
    examples.)


The Examples
------------

The remaining sections have been given over to various standalone
examples. All bar the first are taken from the package itself, each
contributing to either the API or the tools used to prepare the
documentation and test suites.


Example: log processor: development with macros
-----------------------------------------------

To test regex at scale &mdash; which is to say, developing with
relatively complex  REs &mdash;
[a preprocessor](re-nginx-log-processor.html) for parsing NGINX access
and error logs has been written. Each line of input may be either a line
from an NGINX access log or the event log, producing a standard-format
event log on the output.

As a taster, here is the main script, where each type of line is
recognised by a high-level macro.

%include "examples/re-nginx-log-processor.lhs" "script ::"

Thes macros are based on the standard macros, using
`Text.RE.TestBench` to build the up into the above high-level
scanners with the apropriate
[tests and documentation](https://github.com/iconnect/regex/tree/master/tables).

The RE for recognising the access-log lines is built up here.

%include "examples/re-nginx-log-processor.lhs" "access_re ::"

(N.B., The Test Bench currently requires that we write our REs in Haskell
strings.)

See [the log-processor program sources](re-nginx-log-processor.html) for details.


Example: Scanning REs: Named Captures
-------------------------------------

This package needs to recognise all captures in a regular expression so
that it can associate the named captures with their cature ordinal.

Here is the prototype scanner.

%include "Text/RE/Internal/NamedCaptures.lhs" "scan ::"

Once the package has stabilised it should be rewritten with Alex.

See [Text.RE.Internal.NamedCaptures](NamedCaptures.html) for
details.


Anti-Example: Scanning REs in the TestBench
-------------------------------------------

The [Text.RE.TestBench](TestBench.html) contains an almost
identical parser to the above, written with recursive functions.

%include "Text/RE/TestBench.lhs" "scan_re ::"

Once some technical issues have been ersolved it will use the above
scanner in [Text.RE.Internal.NamedCaptures](NamedCaptures.html).


Example: filename analysis
--------------------------

The preprocessor used to prepare the literate programs for this
package's website uses the following 'gen_all' diriver which uses
REs to analyse file paths.

%include "examples/re-prep.lhs" "^gen_all ::"

See [examples/re-prep.lhs](re-prep.html)


Example: parsing RE macros
--------------------------

The regex RE macros are parsed with code that looks similar to
this.

\begin{code}
-- | expand the @{..} macos in the argument string using the given
-- function
expandMacros_ :: (MacroID->Maybe String) -> String -> String
expandMacros_ lu = fixpoint e_m
  where
    e_m re_s =
        replaceAllCaptures TOP phi $ re_s *=~ [re|@$(@|\{${name}([^{}]+)\})|]

    phi mtch _ cap = case txt == "@@" of
        True  -> Just   "@"
        False -> Just $ fromMaybe txt $ lu ide
      where
        txt = capturedText cap
        ide = MacroID $ capturedText $ capture [cp|name|] mtch

fixpoint :: (Eq a) => (a->a) -> a -> a
fixpoint f = chk . iterate f
  where
    chk (x:x':_) | x==x' = x
    chk xs               = chk $ tail xs
\end{code}

For example:
\begin{code}
evalme_PMC_00 = checkThis "evalme_PMC_00" ("foo MacroID {getMacroID = \"bar\"} baz") $ expandMacros_ (Just . show) "foo @{bar} baz"
\end{code}

See [Text.RE.Replace](Replace.html) for details.


Example: Parsing Replace Templates
----------------------------------

The regex replacement templates are parsed with code similar to this.

\begin{code}
type Template = String

parseTemplateE' :: Template
                -> Match String
                -> Location
                -> Capture String
                -> Maybe String
parseTemplateE' tpl mtch _ _ =
    Just $ replaceAllCaptures TOP phi $
      tpl *=~ [re|\$${arg}(\$|[0-9]+|\{${name}([^{}]+)\})|]
  where
    phi t_mtch _ _ = case t_mtch !$? [cp|name|] of
      Just cap -> this $ IsCaptureName $ CaptureName txt
        where
          txt = T.pack $ capturedText cap
      Nothing -> case t == "$" of
        True  -> Just t
        False -> this $ IsCaptureOrdinal $ CaptureOrdinal $ read t
      where
        t = capturedText $ capture [cp|arg|] t_mtch

        this cid = capturedText <$> mtch !$? cid

my_replace :: RE -> Template-> String -> String
my_replace rex tpl src = replaceAllCaptures TOP (parseTemplateE' tpl) $ src *=~ rex
\end{code}

It can be tested with our date-reformater example.

\begin{code}
date_reformat :: String -> String
date_reformat = my_replace [re|${y}([0-9]{4})-${m}([0-9]{2})-${d}([0-9]{2})|] "${y}/${m}/${d}"
\end{code}

This should yield `"2016/01/11"`:

\begin{code}
evalme_TPL_00 = checkThis "evalme_TPL_00" ("2016/01/11") $ date_reformat "2016-01-11"
\end{code}

See [Text.RE.Replace](Replace.html)


Example: include preprocessor
-----------------------------

The 'include' preprocessor for extracting literate programming fragments
(used in this and most of the other sections of the tutorial) has been
lifted out of the main preprocessor into its own example.

Here is sed script that makes up the main loop.

%include "examples/re-include.lhs" "loop ::"

The `extract` action takes the path to the file containing the fragment
and the RE that will match a line in the fragment and returns the text
of the fragment (wrapped in a simple styling div).

%include "examples/re-include.lhs" "extract ::"

And here is the scanner for recognising the literate fragments.

%include "examples/re-include.lhs" "scan ::"

See [examples/re-include.lhs](re-include.html)


Example: literate preprocessor
------------------------------

The preprocessor that converts this literate Haskell program into a web
page and a test suite that makes plenty of use of regex is in
[examples/re-prep.lhs](re-prep.html).


Example: gen-modules
--------------------

The many TDFA and PCRE API modules (but _not_ the `RE` modules) are all
generated from `Text.RE.TDFA.ByteString.Lazy` with
[examples/re-gen-modules.lhs](re-gen-modules.html) which is also an
application of regex.


\begin{code}
main :: IO ()
main = runTests
  [ evalme_TPL_00
  , evalme_PMC_00
  , evalme_REU_01
  , evalme_OPT_01
  , evalme_OPT_00
  , evalme_MAC_00
  , evalme_RPF_02
  , evalme_RPF_01
  , evalme_SOP_06
  , evalme_SOP_05
  , evalme_SOP_04
  , evalme_SOP_03
  , evalme_SOP_02
  , evalme_SOP_01
  , evalme_RPL_02
  , evalme_RPL_01
  , evalme_MLT_03
  , evalme_MLT_02
  , evalme_MLT_01
  , evalme_SGL_02
  , evalme_SGL_01
  , evalme_TRD_04
  , evalme_TRD_02
  , evalme_TRD_01
  , evalme_TRD_00
  , evalme_LOA_00
  ]
\end{code}

