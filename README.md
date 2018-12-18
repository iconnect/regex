# regex

regex is a regular expression toolkit for regex-base with:

  * a text-replacement toolkit with type-safe text-replacement templates;
  * special datatypes for matches and captures;
  * compile-time checking of RE syntax;
  * a unified means of controlling case-sensitivity and multi-line options;
  * high-level AWK-like tools for building text processing apps;
  * the option of using match operators with reduced polymorphism on the
    text and result types;
  * regular expression macros including:
      + a number of useful RE macros;
      + a test bench for testing and documenting new macro environments;
  * built-in support for the TDFA and PCRE back ends;
  * comprehensive documentation, tutorials and copious examples.


See the [About page](http://about.regex.uk) for details.


## regex and regex-examples

The library and tutorial, tests and examples have been split across
two packages:

  * the `regex` package contains the regex library with the Posix TDFA
    back end
  * the `regex-with-pcre` library package contains the extra modules
    needed for the PCRE back end
  * the `regex-examples` package contains the tutorial, tests
    and example programs.


## Road Map

- [X] 2017-04-10  v1.0.0.0  [First stable release](https://github.com/iconnect/regex/milestone/3)
- [X] 2017-06-03  v1.0.1.0  [PCRE.Text, strict PVP, Update Stackage vrns, add re-top](https://github.com/iconnect/regex/milestone/19)
- [X] 2017-06-04  v1.0.1.1  [Fix 1.0.1.0 release bug and provisionally fix UTF8/PCRE interworking](https://github.com/iconnect/regex/milestone/20)
- [X] 2017-06-05  v1.0.1.2  [Permit utf8-string-1](https://github.com/iconnect/regex/milestone/21)
- [X] 2017-06-05  v1.0.1.3  [Suspend Windows tests for PCRE on UTF-8 text](https://github.com/iconnect/regex/milestone/22)
- [X] 2018-12-14  v1.0.1.4  [Fix for GHC 8.4.4, GHC-8.6.2](https://github.com/iconnect/regex/milestone/23)
- [X] 2018-12-18  v1.0.1.5  [TDFA quasi quoters not dealing with \n, etc.](https://github.com/iconnect/regex/milestone/24)


See the [Roadmap page](http://roadmap.regex.uk) for details.


## The regex blog

Check out the [regex blog](http://blog.regex.uk) for news articles and
discussion concerning all things regex.


## Build Status

[![Hackage](http://regex.uk/badges/hackage.svg)](https://hackage.haskell.org/package/regex) [![BSD3 License](http://regex.uk/badges/license.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29) [![Un*x build](http://regex.uk/badges/unix-build.svg)](https://travis-ci.org/iconnect/regex) [![Windows build](http://regex.uk/badges/windows-build.svg)](https://ci.appveyor.com/project/engineerirngirisconnectcouk/regex/branch/master) [![Coverage](http://regex.uk/badges/coverage.svg)](https://coveralls.io/github/iconnect/regex?branch=master)

See [build status page](http://regex.uk/build-status) for details.


## Installing the Package

The package can be easily installed with cabal or stack on GHC-8.0,
 7.10 or 7.8 for the above platforms. See the
[Installation page](http://installation.regex.uk) for details.


## The Tutorial Tests and Examples

See the [Tutorial page](http://tutorial.regex.uk) and
[Examples page](http://examples.regex.uk) for details.


## Helping Out

If you have any feedback or suggestion then please drop us a line.

  * `t` [&#64;hregex](https://twitter.com/hregex)<br/>
  * `e` maintainers@regex.uk<br/>
  * `w` http://issues.regex.uk

The [Contact page](http://contact.regex.uk) has more details.


## The API

The Haddocks can be found at http://hs.regex.uk.


## The Macro Tables

The macro environments are an important part of the package and
are documented [here](http://macros.regex.uk).


## The regex.uk Directory

A handy overview of the regex.uk domain can be found
[here](http://directory.regex.uk).


## The Changelog

The `changelog` is posted [here](http://changelog.regex.uk).


## The Authors

This library was written and is currently maintained by
[Chris Dornan](mailto:chris.dornan@irisconnect.com) aka
[&#64;cdornan](https://twitter.com/cdornan)
