[![Hackage](https://img.shields.io/hackage/v/regex.svg)](https://hackage.haskell.org/package/regex)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)
[![Un*x build](https://img.shields.io/travis/iconnect/regex.svg?label=Linux%2BmacOS)](https://travis-ci.org/iconnect/regex)
[![Windows build](https://img.shields.io/appveyor/ci/engineerirngirisconnectcouk/regex.svg?label=Windows)](https://ci.appveyor.com/project/engineerirngirisconnectcouk/regex/branch/master)
[![Coverage](https://img.shields.io/coveralls/iconnect/regex.svg)](https://coveralls.io/github/iconnect/regex?branch=master)

# regex: A Regular Expression Toolkit for regex-base

regex extends regex-base with:

  * a text-replacement toolkit
  * special datatypes for many matches, first match and individual captures
  * compile-time checking of RE syntax
  * a unified means of controlling case-sensitivity and multi-line options
  * high-level AWK-like tools for building text processing apps
  * the option of using match operators with reduced polymorphism on the
    text and/or result types
  * regular expression macros including
      + a number of useful RE macros
      + a test bench for testing and documenting new macro environments
  * built-in support for the TDFA and PCRE backends
  * comprehensive documentation and copious examples


Schedule
--------

- [X] **2017-01-26**&nbsp;&nbsp;0.0.0.1&nbsp;&nbsp;Pre-release (I)
- [X] **2017-01-30**&nbsp;&nbsp;0.0.0.2&nbsp;&nbsp;Pre-release (II)
- [ ] **2017-02-06**&nbsp;&nbsp;0.0.1.0&nbsp;&nbsp;RFC
- [ ] **2017-02-20**&nbsp;&nbsp;0.1.0.0&nbsp;&nbsp;a candidate stable release
- [ ] **2017-03-20**&nbsp;&nbsp;1.0.0.0&nbsp;&nbsp;first stable release


The Web Page
------------

We have a [web page](https://iconnect.github.io/regex/) with a tutorial,
a major example and and more examples than you can shake a stick at (most
of them used in the package itself).


The Macro Tables
----------------

The macro environments are an important part of the package and they
are documented [in these tables](tables).
