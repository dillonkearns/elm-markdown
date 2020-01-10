# Changelog [![Elm package](https://img.shields.io/elm-package/v/dillonkearns/elm-markdown.svg)](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest/)

All notable changes to
[the `dillonkearns/elm-markdown` elm package](http://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest)
will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [2.0.0] - 2020-01-09

### Fixed

- Parse numbered lists (see [#9](https://github.com/dillonkearns/elm-markdown/pull/9)). Thank you [@stephenreddek](https://github.com/stephenreddek)!!!
- Fix HTML ordering bug (see [#16](https://github.com/dillonkearns/elm-markdown/pull/16)). Thank you [matheus23](https://github.com/matheus23)! 🎉🙏

### Changed
- Give error when there is invalid whitespace within link markdown, see [#10](https://github.com/dillonkearns/elm-markdown/pull/10). Thank you [thomasin](https://github.com/thomasin)!!!

## [1.1.3] - 2019-11-13

### Fixed

- Lists with markers besides `-` are now handled, thanks to
  [#8](https://github.com/dillonkearns/elm-markdown/pull/8) (thank you Stephen Reddekopp 🙏)

## [1.1.2] - 2019-11-12

### Fixed

- HTML attributes were cut short with certain escape characters. They are now correctly parsed, thanks to
  [#11](https://github.com/dillonkearns/elm-markdown/pull/11) (thank you Brian Ginsburg!!!)
