%heading#tutorial The regex Tutorials

We have five tutorials, each of which is a literate Haskell program
with interactive examples that can be tried out with ghci.

  * The [**regex tutorial**](re-tutorial.html) covers basic usage only.
    (**This tutorial is incomplete.**)

  * The [**regex replacing tutorial**](re-tutorial-replacing.html) covers
    the general regex replacement toolkit including a detailed look
    at the `Matches` and `Match` types. (**This tutorial is incomplete.**)

  * The [**regex options tutorial**](re-tutorial-options.html) covers
    the different ways of specifying configuring RE parsing and
    compilation. (**This tutorial is incomplete.**)

  * The [**regex tools tutorial**](re-tutorial-tools.html) looks at the
    regex tools, including a detailed look at the `IsRegex` class.
    (**This tutorial is incomplete.**)

  * The [**regex testbench tutorial**](re-tutorial-testbench.html) looks
    at the regex test bench through the `re-nginx-log-processor`
    example.  (**This tutorial is incomplete.**)


%heading#cabaltutorial Loading the Tutorial with Cabal

%include "lib/md/load-tutorial-cabal-incl.md"


%heading#stacktutorial Loading the Tutorial with Stack

%include "lib/md/load-tutorial-stack-incl.md"
