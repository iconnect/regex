[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)
[![Hackage](https://img.shields.io/hackage/v/regex.svg)](https://hackage.haskell.org/package/regex)
[![Build Status](https://travis-ci.org/iconnect/regex.svg?branch=master)](https://travis-ci.org/iconnect/regex)
[![Build status](https://ci.appveyor.com/api/projects/status/9gqs37u3h1mlc02b/branch/master?svg=true)](https://ci.appveyor.com/project/engineerirngirisconnectcouk/regex/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/iconnect/regex/badge.svg?branch=master)](https://coveralls.io/github/iconnect/regex?branch=master)

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

  * **2017-01-27**&nbsp;&nbsp;0.0.0.1&nbsp;&nbsp;Pre-release
  * **2017-01-30**&nbsp;&nbsp;0.0.1.0&nbsp;&nbsp;RFC
  * **2017-02-20**&nbsp;&nbsp;0.1.0.0&nbsp;&nbsp;a candidate stable release
  * **2017-03-20**&nbsp;&nbsp;1.0.0.0&nbsp;&nbsp;first stable release


The Web Page
------------

We have a [web page](https://iconnect.github.io/regex/) with a tutorial,
a major example and and more examples than you can shake a stick at (most
of them used in the package itself).


The Macro Tables
----------------

The macro environments are an important part of the package and they
are documented [in these tables](tables).
