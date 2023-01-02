# Changelog [![Elm package](https://img.shields.io/elm-package/v/dillonkearns/elm-markdown.svg)](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest/)

All notable changes to
[the `dillonkearns/elm-markdown` elm package](http://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest)
will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [7.0.1] - 2023-01-02

### Fixed
- Fix a corner case for parsing block quotes, see [#127](https://github.com/dillonkearns/elm-markdown/pull/127). Thank you [LutSa](https://github.com/LutSa)!
- Fix parsing of `<` and `>` characters within HTML attributes to treat them as plain characters instead of closing HTML element, see [#130](https://github.com/dillonkearns/elm-markdown/pull/130). Thank you [LutSa](https://github.com/LutSa)!


## [7.0.0] - 2021-08-20

### Added

- We now have nested list parsing! 🎉 A huge thanks to [@LutSa](https://github.com/LutSa) and [@klaftertief](https://github.com/klaftertief) for their work on this! See [#88](https://github.com/dillonkearns/elm-markdown/pull/88).

### Fixed

- Multiline list items are parsed correctly (fixes [#81](https://github.com/dillonkearns/elm-markdown/issues/81)).
- Fixed precedence - thematic breaks take precedence over new list item (fixes [#59](https://github.com/dillonkearns/elm-markdown/issues/59)).

## [6.0.1] - 2021-03-16

### Fixed

- Made some additional whitespace spec compliance around headings, see [#77](https://github.com/dillonkearns/elm-markdown/pull/77). Thank you [@LutSa](https://github.com/LutSa)!

## [6.0.0] - 2021-02-09

### Added

- Now parses strikethroughs (~~like this~~). Thank you [@tadityar](https://github.com/tadityar) for the pull request! See [#75](https://github.com/dillonkearns/elm-markdown/pull/75).

Breaking changes:

`Markdown.Inline.Block` adds

```elm
            | Strikethrough (List.List Markdown.Block.Inline)
```

`Markdown.Renderer` now has a new field:

```elm
            , strikethrough : List.List view -> view
```

## [5.1.1] - 2020-10-28

- Handle some new code fence parsing corner cases. Thank you for the fixes Thomasin! See https://github.com/dillonkearns/elm-markdown/pull/69).

## [5.1.0] - 2020-10-08

### Added

- Inline fold function can do things like gather all links in the markdown. See [#53](https://github.com/dillonkearns/elm-markdown/pull/53) (thank you [@bburdette](bburdette)!).
- Setext headers are now parsed. See [#65](https://github.com/dillonkearns/elm-markdown/pull/65) (thank you [@thomasin](thomasin)!).
- Allow for leading spaces in front of heading hashes, as per markdown spec. See [#66](https://github.com/dillonkearns/elm-markdown/pull/66) (thank you [@pkeugine](pkeugine)!).

## [5.0.0] - 2020-09-09

### Added

- GitHub-Flavored Markdown tables are now parsed! Thank you Stephen! (See [#52](https://github.com/dillonkearns/elm-markdown/pull/52)).

### Changed

- The type for GitHub tables has been fixed. It was missing an outer `List` type. `List (List (List Inline))` refers to
  rows of columns, with a list of inlines in each cell. For example, a cell could have Hello _world_, which would be
  two inlines (the plain text, followed by the italic text).

## [4.0.0] - 2020-03-17

### Changed

- Block data type has been changed to better reflect the naming in the markdown spec.
- The Renderer code is now in a separate module, `Markdown.Renderer`.
- Heading levels are now given as a custom type to make it explicit that the values can't be greater than level 6. There's a helper function in the Block module to convert it to an Int.
- Some names in the `Renderer` record type have changed to better reflect the markdown spec's terminlogy. The `Renderer` also now has functions for displaying Github-Flavored
  Markdown tables. Take a look at this diff to see an example of how to upgrade your Renderer code: https://github.com/dillonkearns/elm-markdown/pull/35/files#diff-5d05b9d569f6fb96977355f9ff688eb4.
  Also note that Table parsing is only partially implemented now so it isn't currently turned on. The actual table parsing functionality will be coming in a future release. But you can handle the types now so we don't need to do a breaking change to introduce that functionality.

### Fixed

This release includes [a lot of new passing specs](https://github.com/dillonkearns/elm-markdown/pull/35/files#diff-3a49125c58477a39487c1c1ef69be134)! Big thanks to Pablo Hirafuji for
his fantastic work on inline parsing in his markdown library, and for giving me
permission to use it here 🙏

- Several parsing cases now fall back to inline parsing rather than giving an error.
- Inline parsing is totally revamped. Including
  - Autolinks (no GFM autolinks yet)
  - Backslash escaping support
  - Hard line breaks
  - Link references now work! (although link reference definitions that are defined in container blocks, like block quotes, are ignored. For now they must be defined at the top level, but they can be referenced anywhere.)
  - Fallbacks work correctly for inline parsing (inlines will never cause the entire parser to fail now. In the future, it may include warnings, but you'll be able to render the fallback.)
  - Some basic inline HTML parsing (doesn't yet support multi-line inline HTML, only multi-line HTML blocks, i.e. the HTML tag must be the first thing on the line).
- HTML comments, CDATA, processing instructions, and declarations are now parsed. They are not rendered, but they are available in the Block structure of your parsed AST. If you wanted to render them, you could transform your AST to change them to a rendering block.

## [3.0.0] - 2020-02-13

### Fixed

- Added parsing for block quotes
- Added task list item parsing (breaking change: see [an example here](https://github.com/dillonkearns/elm-markdown/blob/ed1bbe6f653e77d75a1b86710813a6b3c21a4cec/examples/src/ElmUi.elm#L198-L222), or [the full diff including ElmUi.elm here](https://github.com/dillonkearns/elm-markdown/compare/2.0.2...bc51469?expand=1#diff-5d05b9d569f6fb96977355f9ff688eb4L1-R231)).
- Lines with only tabs or spaces no longer cause parsing errors. Instead, they go through the correct
  blank line handling. See [#28](https://github.com/dillonkearns/elm-markdown/pull/28).
- Fixed some other various cases, see [the new test cases that are passing](https://github.com/dillonkearns/elm-markdown/compare/2.0.2...bc51469?expand=1#diff-3a49125c58477a39487c1c1ef69be134)

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
