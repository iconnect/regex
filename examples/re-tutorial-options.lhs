The regex Options Tutorial
==========================

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


For this module we will work with the PCRE nativeoptions which are
based on bit masks, so `Data.Bits` will be needed.

`Text.RE.REOptions` provides the generic regex types and functions for
handling options, regardless of the selected back end.

Note that we import the PCRE `String` APi `Text.RE.PCRE.String` *and*
the general regex PCRE back end, `Text.RE.PCRE`, needed for the the types
and functions is supplies for accessing the PCRE native options. We could
have just imported `Text.RE.PCRE` but it is useful to see which
extra types and functions being used from this module
(they will be qualified with `PCRE.`).

We also import `Text.Regex.PCRE` from the `regex-pcre` package for the
native pcre-regex types and functions themselves.

\begin{code}
import           Data.Bits
import           TestKit
import qualified Text.RE.PCRE                 as PCRE
import           Text.RE.PCRE.String
import           Text.RE.REOptions
import           Text.Regex.PCRE
\end{code}


Compiling REs with the Complete REOptions
-----------------------------------------

Each `regex-tools` back end &mdash; TDFA and PCRE &mdash; has it own
complile-time options and execution-time options, called in each case
`CompOption` and `ExecOption`. The SimpleREOptions selected with the
RE parser, e.g.,
\begin{code}
evalme_REO_01 = checkThis "evalme_REO_01" (1) $ countMatches $ "0a\nbb\nFe\nA5" *=~ [reBlockInsensitive|[0-9a-f]{2}$|]
\end{code}
configures the RE back end accordingly so that you don't have to,
but you may need full access to your chosen back end's options, in which
case you will need to know about the `REOptions` type, defined by each of
the back ends in terms of the `REOptions_` type of `Text.RE.REOptions`
as follows.
<div class='inlinecodeblock'>
```
type REOptions = REOptions_ RE CompOption ExecOption
```
</div>
(Bear in mind that `RE`, `CompOption` and `ExecOption` are defined
differently in the TDFA and PCRE back ends.)

The `REOptions_` type is defined in `Text.RE.REOptions` as follows:

%include "Text/RE/REOptions.lhs" "data REOptions_"

  * `optionsMacs` contains the macro definitions used to compile
    the REs (see the [test bench tutorial](re-tutorial-testbench.html)
    for details on how to define your own macro environments);

  * `optionsComp` contains the back end compile-time options;

  * `optionsExec` contains the back end execution-time options.

(For more information on the options provided by the back ends see the
decumentation for the `regex-tdfa` and `regex-pcre` packages as
appropriate.)

Each back end provides a function to compile REs from some options and a
string containing the RE as follows:
<div class='inlinecodeblock'>
```
compileRegexWithOptions :: (IsOption o, Functor m, Monad   m)
                        => o
                        -> String
                        -> m RE
```
</div>
where `o` is one of the following RE-configuring types:

  * `()` (the unit type), representing the default multi-line
    case-sensitive used with the `re` parser.

  * `SimpleREOptions` (explained in the [main tutorial](re-tutorial.html)),
    which will be converted into the appropriate `CompOption` and
    `ExecOption` for the beck end in question);

  * `CompOption` to directly specify the compile-time options for the back
    end;

  * `ExecOption` to specify the execution-time options for the
    back end;

  * `Macros RE` to specify the alternative macros to use instead of the
    standard environment;

  * `REOptions` to specify all together, the back-end options and the
    macro table to use.

The compilation takes place in a monad to allow for failure. In the following
examples we will use this helper, which will extract the compiled RE using
error to deal with any failures.
\begin{code}
check :: Maybe a -> a
check = maybe (error "booyah") id
\end{code}

\begin{code}
evalme_OPT_01 = checkThis "evalme_OPT_01" (1) $ countMatches $ "0a\nbb\nFe\nA5" *=~ check (compileRegexWith BlockInsensitive "[0-9a-f]{2}$")
\end{code}

This will allow you to compile regular expressions when the either the
text to be compiled or the options have been dynamically determined.

If you need to build `SearchReplace` templates then there is an analagous
compilation function for that:

```
compileSearchReplaceWithOptions :: (Monad m,Functor m,IsRegex RE s)
                                => REOptions
                                -> String
                                -> String
                                -> m (SearchReplace RE s)
```


Specifying REOptions with `re_` and `ed_`
-----------------------------------------

If you just need to specify some non-standard options with a static RE,
you can use the `re_` quasi quoter, which yields a function takes an
option type and returns the RE compiled with the given options:
\begin{code}
evalme_REU_01 = checkThis "evalme_REU_01" (1) $ countMatches $ "0a\nbb\nFe\nA5" *=~ [re_|[0-9a-f]{2}$|] BlockInsensitive
\end{code}
Any option `o` such that `IsOption o RE CompOption ExecOption` (i.e.,
any option type accepted by `compileRegex` above) can be used with
`[re_` ... `|]`.

The `[ed_` ... `///` ... `|]` for compiling `SearchReplace` templates
works analagously, yielding a function that takes an option type and
returns the `SearchReplace` template comoiled with those RE options.


Configuring Native (PCRE) Options
---------------------------------

The function `unpackSimpleREOptions`, used to generate PCRE native
options from the generic `SimpleREOptions` is defined like this.
(We have made some minor organizational changes for this
presentaion, but this is equivalent to the library code used for
`PCRE.unpackSimpleREOptions`.)

\begin{code}
unpackSimpleREOptions :: SimpleREOptions -> PCRE.REOptions
unpackSimpleREOptions sro =
  REOptions
    { optionsMacs = PCRE.prelude      -- the standard 'prelude' macro environment
    , optionsComp = comp              -- our calculated PCRE compile options
    , optionsExec = defaultExecOpt    -- the default PCRE run-time options
    }
  where
    comp =
      wiggle ml compMultiline $
      wiggle ci compCaseless
        defaultCompOpt

    (ml,ci) = case sro of
        MultilineSensitive    -> (,) True  False
        MultilineInsensitive  -> (,) True  True
        BlockSensitive        -> (,) False False
        BlockInsensitive      -> (,) False True

-- set or clear a PCRE option bit according to the
-- Bool in its first argument using the bit mask
-- passed in the second argument
wiggle :: Bits a => Bool -> a -> a -> a
wiggle True  m v = v .|.            m
wiggle False m v = v .&. complement m
\end{code}

Now we will set up a apecial set of PCRE options based on
`BlockInsensitive`, but with the
[PCRE `DOTALL` option bit](http://pcre.org/pcre.txt) set.

\begin{code}
myOptions :: PCRE.REOptions
myOptions =
  PCRE.defaultREOptions
    { optionsComp = wiggle True compDotAll $ optionsComp biOptions
    }

biOptions :: PCRE.REOptions
biOptions = unpackSimpleREOptions BlockInsensitive
\end{code}

Now we can test `myOptions` with the `[re_| ... |]` quasi quoter as follows.

\begin{code}
evalme_EXA_01 = checkThis "evalme_EXA_01" (True) $ matched $ "0a\nbbxFe&A5 " ?=~ [re_|^([0-9a-f]{2}.){4}$|] myOptions
\end{code}

That test matched, but if we provide just `BlockInsensitive` options set up
in `biOptions` above,

\begin{code}
evalme_EXA_02 = checkThis "evalme_EXA_02" (False) $ matched $ "0a\nbbxFe&A5 " ?=~ [re_|^([0-9a-f]{2}.){4}$|] biOptions
\end{code}

the match fails.

\begin{code}
main :: IO ()
main = runTheTests
  [ evalme_EXA_02
  , evalme_EXA_01
  , evalme_REU_01
  , evalme_OPT_01
  , evalme_REO_01
  ]
\end{code}

