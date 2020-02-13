# Changelog [![Elm package](https://img.shields.io/elm-package/v/dillonkearns/elm-markdown.svg)](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest/)

All notable changes to
[the `dillonkearns/elm-markdown` elm package](http://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest)
will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed
- Added parsing for block quotes
- Added task list item parsing (breaking change: see [an example here](https://github.com/dillonkearns/elm-markdown/blob/ed1bbe6f653e77d75a1b86710813a6b3c21a4cec/examples/src/ElmUi.elm#L198-L222), or [the full diff including ElmUi.elm here](https://github.com/dillonkearns/elm-markdown/compare/2.0.2...bc51469?expand=1#diff-5d05b9d569f6fb96977355f9ff688eb4L1-R231)).
- Lines with only tabs or spaces no longer cause parsing errors. Instead, they go through the correct
    blank line handling. See [#28](https://github.com/dillonkearns/elm-markdown/pull/28).
- Fixed some other various cases, see [the new test cases that are passing]( https://github.com/dillonkearns/elm-markdown/compare/2.0.2...bc51469?expand=1#diff-3a49125c58477a39487c1c1ef69be134)

## [2.0.2] - 2020-02-07

### Fixed
- Got a 16x performance improvement thanks to
    [this performance optimization trick](https://discourse.elm-lang.org/t/performance-optimization/5105)!

## [2.0.1] - 2020-01-21

### Fixed
- Just pushing README update with updated Ellie examples, see [#22](https://github.com/dillonkearns/elm-markdown/issues/22).

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
