The regex Test-Bench Tutorial
=============================

Language Options and  Imports
-----------------------------

This tutorial is a literate Haskell program whwre we start by specifying
the language pragmas and imports we will need for this module.

\begin{code}
{-# LANGUAGE QuasiQuotes                      #-}
{-# LANGUAGE FlexibleContexts                 #-}
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
import           Data.Functor.Identity
import qualified Data.HashMap.Lazy                        as HML
import           TestKit
import           Text.RE.REOptions
import qualified Text.RE.TDFA                             as TDFA
import           Text.RE.TDFA.String
import           Text.RE.TestBench
\end{code}


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


Adding the Epsilon Macro
------------------------

You can use the regex test bench to add you own macros. As a simple example
we will add an 'epsilon' macro to the standard 'prelude' macro environment.
(See the [`re-nginx-log-processor`](re-nginx-log-processor) for a more
extensive example of macro environments.)

The `@{epsilon}` macro will expand to a RE that matches only the empty
string:
```
.{0}
```

(A use such a seemingly useless RE macro will become apparent in the
test example below.)

Firstly we define a two argument function function to create a `MacroDescriptor`
from:

  1. the `MacroEnv` macro environment argument will be used to compile
     the macro RE (we don't need it in this instance, of course,
     but we are following a general recipe);

  2. the `macroId` name of the macro (which is passed into us because
     the calling context need the name of the macro).

\begin{code}
epsilon_macro :: MacroEnv -> MacroID -> MacroDescriptor
epsilon_macro env mid =
  runTests TDFA.regexType Just samples env mid
    MacroDescriptor
      { macroSource          = RegexSource ".{0}" -- the RE to be substituted for the macro
      , macroSamples         = map fst samples    -- list of string that should match the above macro RE
      , macroCounterSamples = counter_samples     -- list of string that should **not** match the above macro RE
      , macroTestResults    = []                  -- for bookkeeping
      , macroParser          = Nothing            -- no parser needed for this one!
      , macroDescription     = "an epsilon parser, matching the empty string only"
      }
  where
    samples :: [(String,String)]
    samples =
        [ dup ""
        ]
      where
        dup x = (x,x)

    counter_samples =
        [ "not an empty string"
        ]
\end{code}

The compiled `Macros RE` that we will slot into the `REOptions` used to
compile the RE is constructed in two steps. Firstly we provide a function
that takes the @MacroEnv@ that all of the macros will use to build their
REs and returns the augmented `MacroEnv` with the new macro definitions.

This `MacroEnv` is generic and not dependent upon any back end &mdash;
none of the macros have been compiled.
\begin{code}
my_env :: MacroEnv -> MacroEnv
my_env env0 = env
  where
    env = env0 `HML.union` HML.fromList
      [ f "epsilon" epsilon_macro
      ]

    f nm mk = (mid, mk env mid)
      where
        mid = MacroID nm
\end{code}

From the `MacroEnv` we compile the macros into a `Macros RE` macro table
that we can insert into an `REOptions` that can be used to compile REs
in the application.
\begin{code}
my_macros :: Macros RE
my_macros = runIdentity $ mkMacros mk TDFA.regexType ExclCaptures $ my_env TDFA.preludeEnv
  where
    mk   = maybe oops Identity . TDFA.compileRegexWithOptions TDFA.noPreludeREOptions

    oops = error "my_macros: unexpected RE compilation error"
\end{code}

The `makeREOptions` function can be used to construct an `REOptions`
for compiling REs with `[re_| ... |]` and `[ed_| ... /// ... |]` quasi
quoters.

\begin{code}
myOptions :: TDFA.REOptions
myOptions = TDFA.makeREOptions my_macros
\end{code}

Now we can try out the `@{epsilon}` macro, using it to match nothing!

\begin{code}
evalme_TST_00 = checkThis "evalme_TST_00" (True) $ matched $ "///" ?=~ [re_|^//@{epsilon}/$|] myOptions
\end{code}

Why would we we want to match nothing? To break up three '/' in the RE part
of a `[ed_| ... /// ... |]` `SearchReplace` template.

\begin{code}
evalme_TST_01 = checkThis "evalme_TST_01" ("a <three slashes> replacement example") $ "a <///> replacement example" *=~/ [ed_|<//@{epsilon}/>///<three slashes>|] myOptions
\end{code}

For a more extensive example of macro environments see the
[`re-nginx-log-processor`](re-nginx-log-processor)

\begin{code}
main :: IO ()
main = runTheTests
  [ evalme_TST_01
  , evalme_TST_00
  , evalme_MAC_00
  ]
\end{code}

