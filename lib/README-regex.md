# regex

regex is a regular expression toolkit for regex-base with:

  * text-replacement operations with named captures;
  * special datatypes for matches and captures;
  * compile-time checking of RE syntax;
  * a unified means of controlling case-sensitivity and multi-line options;
  * high-level AWK-like tools for building text processing apps;
  * the option of using match operators with reduced polymorphism on the
    text and/or result types;
  * regular expression macros including:
      + a number of useful RE macros;
      + a test bench for testing and documenting new macro environments;
  * built-in support for the TDFA and PCRE backends;
  * comprehensive documentation and copious examples.


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

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-01-26  v0.0.0.1  Pre-release (I)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-01-30  v0.0.0.2  Pre-release (II)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-02-18  v0.1.0.0  [Proposed core API with presentable Haddocks](https://github.com/iconnect/regex/milestone/1)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-02-19  v0.2.0.0  [Package split into regex and regex-examples](https://github.com/iconnect/regex/milestone/5)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-02-20  v0.2.0.1  [Remove library from regex-examples](https://github.com/iconnect/regex/issues/43)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-02-21  v0.2.0.2  [Fix tests for Hackage](https://github.com/iconnect/regex/issues/45)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-02-21  v0.2.0.3  Tweak README/index layout

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-02-22  v0.2.0.4  [Repair re-gen-modules-test for Windows](https://github.com/iconnect/regex/issues/47)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-02-26  v0.3.0.0  [API adjustments](https://github.com/iconnect/regex/milestone/2)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-03-05  v0.5.0.0  [Ready for review: API, tutorials and examples finalized](https://github.com/iconnect/regex/issues/67)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-03-13  v0.6.0.0  [Split out PCRE](https://github.com/iconnect/regex/milestone/7)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-03-13  v0.6.0.1  [Fix .travis.yml release-stack script](https://github.com/iconnect/regex/issues/67)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-03-15  v0.7.0.0  [Better organization of API](https://github.com/iconnect/regex/milestone/8)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-03-16  v0.8.0.0  [Tidy up the API](https://github.com/iconnect/regex/milestone/10)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-03-24  v0.9.0.0  [Finish tidying up the API, Add type-safe replacement templates and exploit TemplateHaskellQuotes](https://github.com/iconnect/regex/milestone/9)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-03-27  v0.10.0.2 [Tweak Templates, Fix release scripts and update Haddocks commentary](https://github.com/iconnect/regex/milestone/12)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-03-28  v0.10.0.3 [Upgrade to LTS 8.6 and Improve Haddocks for Text.RE.{TDFA,PCRE}](https://github.com/iconnect/regex/milestone/13)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2612;&nbsp;&nbsp;2017-03-29  v0.11.0.0 [Simplify the API](https://github.com/iconnect/regex/milestone/14)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2610;&nbsp;&nbsp;2017-03-31  v1.0.0.0  [First stable release](https://github.com/iconnect/regex/milestone/3)

&nbsp;&nbsp;&nbsp;&nbsp;&#x2610;&nbsp;&nbsp;2017-08-31  v2.0.0.0  [Fast text replacement with benchmarks](https://github.com/iconnect/regex/milestone/4)



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

&nbsp;&nbsp;&nbsp;&nbsp;`t` [&#64;hregex](https://twitter.com/hregex)\n
&nbsp;&nbsp;&nbsp;&nbsp;`e` maintainers@regex.uk\n
&nbsp;&nbsp;&nbsp;&nbsp;`w` http://issues.regex.uk

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
