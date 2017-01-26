regex
=====

_A Regular Expression Toolkit for regex-base_

regex extends regex-base with:

  * a text-replacement toolkit
  * special datatypes for many matches, first match and individual captures
  * compile-time checking of RE syntax
  * a unified means of controlling case-sensitivity and multi-line options
  * high-level Awk-like tools for building text processing apps
  * the option of using match operators with reduced polymorphism on the
    text and/or result types
  * regular expression macros including
      + a number of useful RE macros
      + a test bench for testing and documenting new macro environments
  * built-in support for the TDFA and PCRE backends
  * comprehensive documentation and copious examples


Schedule
--------

  * 2017-01-27 0.0.0.1 Pre-release
  * 2017-01-30 0.0.1.0 RFC
  * 2017-02-20 0.1.0.0 a candidate stable release
  * 2017-02-20 1.0.0.0 a first stable release


Installation Instructions
-------------------------

Either

```bash
cabal update && cabal install regex
```

or

```bash
stack install regex
```


Loading up the Tutorial into ghci
---------------------------------

```bash
cabal unpack regex
cd regex-*
cabal configure --enable-tests
cabal repl examples/re-tutorial
```


Table of Contents
=================


## The Tutorial, Tests and Examples

### The Tutorial [examples/re-tutorial.lhs](re-tutorial.html)
provides an introduction to the package with simple examples that you can try
out in your favourite Haskell REPL and references to examples in the example
programs and library code.

### The Log Processor Example [examples/re-nginx-log-processor.lhs](re-nginx-log-processor.html)
provides an extended example of large-scale RE development with the regex test bench.

### The Include Processor Example [examples/re-include.lhs](re-include.html)
is the starting point for the preprocessor that we use to generate the tutorial
HTML and its derived test suite.

### The Library Tests [examples/re-tests.lhs](re-tests.html)
contains various test suites for exercising various components of the library.

### The Tutorial Preprocessor [examples/re-pp.lhs](re-pp.html)
contains the tool we use to generate the tutorial HTML and its derived test suite.

### The API Module Generator [examples/re-gen-modules.lhs](re-gen-modules.html)
contains a tool for generating the parts of the API that can be easily synthesized from a
seed/master module.


## Selected Library Modules

Some of the library modules have been prepared as literate programs for easy
browsing of their underlying source code.

### [Text.RE.Capture](Capture.html)
contains the definitions of the
`Matches`, `Match` and `Capture` data types (with helpers) that form the
foundations for everything else.

### [Text.RE.Replace](Replace.html)
contains the text-replacement toolkit.

### [Text.RE.Options](Options.html)
contains the `Options` types for controlling RE parsing and compilation.

### [Text.RE.IsRegex](IsRegex.html)
contains the IsRegex class for writing polymorphic regex tools that work with
all regex back ends and text type combinations.

### [Text.RE.TestBench](TestBench.html)
contains the test bench used to build the standard macro environment and can be
used for developing other macro environments with test and documentation.

### [Text.RE.Edit](Edit.html)
contains the polymorphic editing toolkit used by `Text.RE.Tools.Sed`.

### [Text.RE.Tools.Sed](Sed.html)
contains the Sed tool for building awk-like text processors.

### [Text.RE.Tools.Grep](Grep.html)
contains a simple grep tool for extracting lines that match a RE from a file.

### [Text.RE.Tools.Lex](Lex.html)
contains a simple scanning tool for building prototype scanners before being
discarded or converted into Alex scanners.

### [Text.RE.Internal.NamedCaptures](NamedCaptures.html)
an internal library module for dealing with named captures in REs.
