%heading#build The Live Build Status

All of the badges elsewhere represent the build status of the current version
of regex in Hackage.  This page however collects the live status of the
Travis-CI and AppVeyor pipelines that monitor the head of the repository.

| Service      | O/S     | Build         | GHC    | LTS  | extra-deps              | Werror        | Build&nbsp;Status
| ------------ | ------- | ------------- | ------ | ---- | ----------------------- | ------------- | -------------
| Hackage      |         |               |        |      |                         | &#8209;Wwarn  | [![Hackage](https://img.shields.io/hackage/v/regex.svg)](https://hackage.haskell.org/package/regex)
| Licence      |         |               |        |      |                         |               | [![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)
| Travis CI    | Linux   | release-stack | 8.0.2  | 8.6  |                         | &#8209;Werror | [![Un*x build](https://travis-ci.org/iconnect/regex.svg?branch=master)](https://travis-ci.org/iconnect/regex)
|              | Linux   | stack         | 7.10.3 | 6.30 |                         | &#8209;Werror |
|              | Linux   | stack         | 7.8.4  | 2.22 | regex-tdfa-text-1.0.0.3 | &#8209;Werror |
|              | Linux   | stack         | 8.0.2  | 8.5  |                         | &#8209;Werror |
|              | Linux   | cabal         | 7.10.3 |      |                         | &#8209;Werror |
|              | Linux   | stack         | 8.0.2  | 2017&#8209;03&#8209;28 |       | &#8209;Werror |
|              | macOS   | stack         | 7.8.4  | 2.22 |                         | &#8209;Werror |
|              | macOS   | stack         | 8.0.2  | 8.5  |                         | &#8209;Werror |
| AppVeyor     | Windows | stack         | 8.0.2  | 8.5  |                         | &#8209;Werror |  [![Windows build](https://ci.appveyor.com/api/projects/status/hmgqoawgptk72epq?svg=true)](https://ci.appveyor.com/project/cdornan/regex)
| coveralls.io | Linux   | stack         | 7.10.3 | 6.30 |                         | &#8209;Werror |  [![Coverage](https://coveralls.io/repos/github/iconnect/regex/badge.svg?branch=master)](https://coveralls.io/github/iconnect/regex?branch=master)


%heading#coveragenote Coverage Exceptions

The following modules have been exempted from the code coverage statistics.

| Module&nbsp;Exempted | Reason                                                                
| -------------------  | -----------------------------------------------------------------------------
| [Text.RE.Internal.QQ](https://github.com/iconnect/regex/blob/master/Text/RE/Internal/QQ.hs) | Toolkit for use in quasi quoter contexts only which can't be measured by hps.


%heading#hackagebuild The regex Hackage Matrix Builder

The regex Hackage Matrix Builder summarizes the buildability of each version of
each Hackage package:

  * [`regex` age](http://104.239.175.197:8080/package/regex)
  * [`regex-with-pcre` page](http://104.239.175.197:8080/package/regex-with-pcre)
  * [`regex-examples` page](http://104.239.175.197:8080/package/regex-examples)
