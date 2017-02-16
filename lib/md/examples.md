%heading#tutorial The Tutorial

The Tutorial [examples/re-tutorial.lhs](re-tutorial.html)
provides an introduction to the package with simple examples that you can try
out in your favourite Haskell REPL and references to examples in the example
programs and library code.


%heading#tests The Test Suite

The Library Tests [examples/re-tests.lhs](re-tests.html)
contains various test suites for exercising various components of the library.


%heading#programs The NGINX Log Processor

The Log Processor Example [examples/re-nginx-log-processor.lhs](re-nginx-log-processor.html)
provides an extended example of large-scale RE development with the regex test bench.


%heading#tools The Regex Tools

  * The Include Processor Example [examples/re-include.lhs](re-include.html)
    is the starting point for the preprocessor that we use to generate the tutorial
    HTML and its derived test suite.

  * The Cabal Processor Example [examples/re-gen-cabals.lhs](re-gen-cabals.html)
    is the Sed preprocessor we use to generate our cabal file from the template
    in [lib/regex-master.cabal](https://github.com/iconnect/regex/blob/master/lib/regex-master.cabal).

  * The Tutorial Preprocessor [examples/re-prep.lhs](re-prep.html)
    contains the tool we use to generate the tutorial HTML and its derived test suite.

  * The API Module Generator [examples/re-gen-modules.lhs](re-gen-modules.html)
    contains a tool for generating the parts of the API that can be easily synthesized from a
    seed/master module.

  * The Cabal-file Generator [examples/re-gen-modules.lhs](re-gen-modules.html)
    contains a tool for generating the cabal file from the template(s) in `lib/cabal-masters`.


%heading#library Selected Library Modules

Some of the library modules have been prepared as literate programs for easy
browsing of their underlying source code.

  * [Text.RE.Capture](Capture.html) contains the definitions of the
    `Matches`, `Match` and `Capture` data types (with helpers) that form the
    foundations for everything else.

  * [Text.RE.Replace](Replace.html) contains the text-replacement toolkit.

  * [Text.RE.Options](Options.html) contains the `Options` types for
    controlling RE parsing and compilation.

  * [Text.RE.IsRegex](IsRegex.html) contains the IsRegex class for writing
    polymorphic regex tools that work with all regex back ends and text
    type combinations.

  * [Text.RE.TestBench](TestBench.html) contains the test bench used to
    build the standard macro environment and can be used for developing
    other macro environments with test and documentation.

  * [Text.RE.Edit](Edit.html) contains the polymorphic editing toolkit
    used by `Text.RE.Tools.Sed`.

  * [Text.RE.Tools.Sed](Sed.html) contains the Sed tool for building
    awk-like text processors.

  * [Text.RE.Tools.Grep](Grep.html) contains a simple grep tool for
    extracting lines that match a RE from a file.

  * [Text.RE.Tools.Lex](Lex.html) contains a simple scanning tool for
    building prototype scanners before being discarded or converted
    into Alex scanners.

  * [Text.RE.Internal.NamedCaptures](NamedCaptures.html)
    is an internal library module for dealing with named captures in REs.
