# Changelog [![Elm package](https://img.shields.io/elm-package/v/dillonkearns/elm-markdown.svg)](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest/)

All notable changes to
[the `dillonkearns/elm-markdown` elm package](http://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest)
will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed 
* HTML attributes were cut short with certain escape characters. They are now correctly parsed, thanks to
     [#11](https://github.com/dillonkearns/elm-markdown/pull/11) (thank you Brian Ginsburg!!!)
