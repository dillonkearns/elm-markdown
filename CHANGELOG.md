# Changelog [![Elm package](https://img.shields.io/elm-package/v/dillonkearns/elm-markdown.svg)](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest/)

All notable changes to
[the `dillonkearns/elm-markdown` elm package](http://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest)
will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

- **`Markdown.Parser.parse` is now infallible.** It returns `List Block` instead of `Result (List DeadEnd) (List Block)`. Any input produces output — malformed syntax is treated as plain text, consistent with how mature markdown parsers behave.
- **`Markdown.Renderer.Renderer` has a new `err` type parameter.** The type is now `Renderer err view` instead of `Renderer view`. The `err` tracks whether the HTML renderer can fail.
- **`Markdown.Renderer.render` now requires `Renderer Never view`** and returns `List view` directly (no `Result`). For renderers that can fail, use the new `tryRender`.
- **`Block.HtmlElement` has a new 4th field** for raw body text: `HtmlElement String (List HtmlAttribute) (List children) String`. The new `String` is the unparsed source text between the opening and closing tags. Pattern matches on `HtmlElement tag attrs children` must become `HtmlElement tag attrs children raw`.
- **`Inline.HtmlInline` now wraps `Html Inline` instead of `Html Block`.** Inline HTML elements now contain inline children, parsed as inline markdown. Code matching on `HtmlInline` children must be updated.
- **`Block.UnorderedList` now carries `ListSpacing`:** `UnorderedList ListSpacing (List (ListItem Block))` (was `UnorderedList (List (ListItem Inline))`).
- **`Block.OrderedList` now carries `ListSpacing`:** `OrderedList ListSpacing Int (List (List Block))` (was `OrderedList Int (List (List Inline))`).

### Added

- **`Markdown.Renderer.tryRender`** — renders with a fallible renderer, returning `Result err (List view)`. This replaces the old `render` for cases where rendering can fail (e.g. unregistered HTML tags).
- **`Markdown.Html.withFallback`** — converts a `Renderer String (List view -> view)` into a `Renderer Never (List view -> view)` by providing a fallback function for unmatched HTML tags. This enables a fully infallible parse-and-render pipeline.
- **`Markdown.Html.withRawContent`** — extracts the raw body text of an HTML tag (the unparsed source between opening and closing tags). Useful for elements like `<style>` or `<script>` where you want the raw text, not parsed markdown.
- **`Block.ListSpacing`** — new exposed type (`Loose | Tight`) replacing implicit list spacing tracking.
- **`Block.inlineFoldl`** — now exposed in the public API for folding over all inlines within a list of blocks.
- 21 additional CommonMark/GFM spec tests now pass (malformed HTML is properly escaped as text, `<script>`/`<style>`/`<textarea>` blocks preserve raw content).
- Fuzz tests and edge-case tests for the infallible parser.

### Fixed

- Fixed `defaultHtmlRenderer` `orderedList` start attribute logic — previously the `start` attribute was incorrectly added for lists starting at 1 and omitted for all other starting indices.
- Fixed swapped `String.endsWith` argument order in block quote lazy continuation logic.

### Removed

- `Markdown.Parser.deadEndToString` — no longer needed since `parse` cannot fail.
- `Block.ClosingTag` variant — stray closing tags are now handled internally.

### Migration Guide

**1. Update `parse` call sites** — remove `Result` handling:

```elm
-- Before
markdown
    |> Markdown.Parser.parse
    |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
    |> Result.andThen (Markdown.Renderer.render renderer)

-- After (infallible renderer)
markdown
    |> Markdown.Parser.parse
    |> Markdown.Renderer.render renderer

-- After (fallible renderer)
markdown
    |> Markdown.Parser.parse
    |> Markdown.Renderer.tryRender renderer
```

**2. Add `err` type parameter to `Renderer` annotations:**

```elm
-- Before
renderer : Markdown.Renderer.Renderer (Html msg)

-- After
renderer : Markdown.Renderer.Renderer String (Html msg)
```

**3. (Optional) Make rendering infallible with `withFallback`:**

```elm
{ myRenderer
    | html =
        Markdown.Html.oneOf [ {- your tag handlers -} ]
            |> Markdown.Html.withFallback
                (\tag attributes { rendered } ->
                    Html.node tag (List.map (\a -> Attr.attribute a.name a.value) attributes) rendered
                )
}
-- This renderer has type `Renderer Never view`, so `render` returns `List view` directly.
```

**4. Update pattern matches on `Block` types:**

```elm
-- HtmlElement: add 4th field
-- Before
HtmlElement tag attrs children -> ...
-- After
HtmlElement tag attrs children raw -> ...

-- UnorderedList: add ListSpacing, children are now Block
-- Before
UnorderedList items -> ...
-- After
UnorderedList spacing items -> ...

-- OrderedList: add ListSpacing, children are now Block
-- Before
OrderedList startIndex items -> ...
-- After
OrderedList spacing startIndex items -> ...
```

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
