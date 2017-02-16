%heading#cabalinstall Installing with Cabal

The regex package is [tested with](build-status) GHC 7.8.4, 7.10.3 and 8.0.1:

```bash
cabal update && cabal install regex
```

%heading#stackinstall Installing with Stack

We maintain three stack configurations:

```bash
stack --stack-yaml stack-8.0.yaml install regex
```

and

```bash
stack --stack-yaml stack-7.10.yaml install regex
```

and

```bash
stack --stack-yaml stack-7.8.yaml install regex
```


%heading#cabaltutorial Loading the Tutorial with Cabal

%include "lib/md/load-tutorial-cabal-incl.md"


%heading#stacktutorial Loading the Tutorial with Stack

%include "lib/md/load-tutorial-stack-incl.md"


%heading#cabaltest Running the tests with Cabal

To run the tests with cabal, change into the root folder and:

```bash
cabal test
```


%heading#stacktest Running the tests with Stack

To test with GHC-8.0 from the root folder:
```bash
stack test --stack-yaml stack-8.0.yaml
```
