%heading#build The Live Build Status

All of the badges elsewhere represent the build status of the current version
of regex in Hackage.  This page however collects the live status of the
Travis-CI and AppVeyor pipelines that monitor the head of the repository.

| Service      | O/S     | Build | GHC    | LTS  | extra-deps              | Status
| ------------ | ------- | ----- | ------ | ---- | ----------------------- | ------
| Hackage      |         |       |        |      |                         | [![Hackage](https://img.shields.io/hackage/v/regex.svg)](https://hackage.haskell.org/package/regex)
| Licence      |         |       |        |      |                         | [![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)
| Travis CI    | Linux   | stack | 7.10.3 | 6.29 |                         | [![Un*x build](https://travis-ci.org/iconnect/regex.svg?branch=master)](https://travis-ci.org/iconnect/regex)
|              | Linux   | stack | 7.8.4  | 2.22 | regex-tdfa-text-1.0.0.3 |
|              | Linux   | stack | 8.0.1  | 7.18 |                         |
|              | Linux   | cabal | 7.10.3 |      |                         |
|              | macOS   | stack | 7.8.4  |      |                         |
|              | macOS   | stack | 8.0.1  |      |                         |
| AppVeyor     | Windows | stack | 8.0.1  | 7.18 |                         |  [![Windows build](https://ci.appveyor.com/api/projects/status/9gqs37u3h1mlc02b?svg=true)](https://ci.appveyor.com/project/engineerirngirisconnectcouk/regex/branch/master)
| coveralls.io |         |       |        |      |                         |  [![Coverage](https://coveralls.io/repos/github/iconnect/regex/badge.svg?branch=master)](https://coveralls.io/github/iconnect/regex?branch=master)
