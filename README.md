# elm-markdown

[![All Contributors](https://img.shields.io/badge/all_contributors-3-orange.svg?style=flat-square)](#contributors)
![Build Status](https://github.com/dillonkearns/elm-markdown/workflows/Elm%20CI/badge.svg) [![Elm package](https://img.shields.io/elm-package/v/dillonkearns/elm-markdown.svg)](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest/)

Extensible markdown parsing in pure elm.

This library extends the basic markdown blocks without actually adding features to the syntax.
It simply provides a declarative way to map certain HTML tags to your Elm view functions to render them. For example,

```html
<bio
  name="Dillon Kearns"
  photo="https://avatars2.githubusercontent.com/u/1384166"
  twitter="dillontkearns"
  github="dillonkearns"
>
  Dillon really likes building things with Elm! Here are some links -
  [Articles](https://incrementalelm.com/articles)
</bio>
```

And you wire up your Elm rendering function like this

```elm
Markdown.Html.oneOf
  [ Markdown.Html.tag "bio"
    (\name photoUrl twitter github dribbble renderedChildren ->
      bioView renderedChildren name photoUrl twitter github dribbble
    )
    |> Markdown.Html.withAttribute "name"
    |> Markdown.Html.withAttribute "photo"
    |> Markdown.Html.withOptionalAttribute "twitter"
    |> Markdown.Html.withOptionalAttribute "github"
    |> Markdown.Html.withOptionalAttribute "dribbble"
  ]
```

Note that it gets the rendered children as an argument. This is rendering the inner contents of the HTML tag using
your HTML renderer, so you get all of your rendered lists, code blocks, links, etc. within your tag. You can try a [live Ellie demo of this code snippet](https://ellie-app.com/cHB3fRSKVRha1).

## Live Code Demos

- [Custom HTML Block Rendering (with `elm-ui`)](https://ellie-app.com/d7R3b9FsHfCa1)
- [Extracting a table of contents from the parsed Markdown](https://ellie-app.com/cHB3fRSKVRha1)
- [Running the built-in, standard markdown HTML renderer](https://ellie-app.com/f4FsH8bHsC6a1)
- [Live Lisp evaluation, with values propogating through multiple Markdown HTML blocks](https://bburdette.github.io/cellme/mdcelldemo.html) - check out the source code at [github.com/bburdette/cellme/blob/master/examples/src/MdMain.elm](https://github.com/bburdette/cellme/blob/master/examples/src/MdMain.elm)

## Core features

### Custom Renderers

You define your own custom renderer, turning your markdown content into any data type with totally customizable logic. You can even pass back an `Err` to get custom failures (for example, broken links or validations like headings that are too long)!

Here's a snippet from the default HTML renderer that comes built in to give you a sense of how you define a `Renderer`:

```elm
import Html exposing (Html)
import Html.Attributes as Attr
import Markdown.Block as Block exposing (Block)
import Markdown.Html

defaultHtmlRenderer : Renderer String (Html msg)
defaultHtmlRenderer =
    { heading =
        \{ level, children } ->
            case level of
                Block.H1 ->
                    Html.h1 [] children

                Block.H2 ->
                    Html.h2 [] children

                Block.H3 ->
                    Html.h3 [] children

                Block.H4 ->
                    Html.h4 [] children

                Block.H5 ->
                    Html.h5 [] children

                Block.H6 ->
                    Html.h6 [] children
    , paragraph = Html.p []
    , hardLineBreak = Html.br [] []
    , blockQuote = Html.blockquote []
    , strong =
        \children -> Html.strong [] children
    , emphasis =
        \children -> Html.em [] children
    , codeSpan =
        \content -> Html.code [] [ Html.text content ]
    , link =
        \link content ->
            case link.title of
                Just title ->
                    Html.a
                        [ Attr.href link.destination
                        , Attr.title title
                        ]
                        content

                Nothing ->
                    Html.a [ Attr.href link.destination ] content
    , image =
        \imageInfo ->
            case imageInfo.title of
                Just title ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        , Attr.title title
                        ]
                        []

                Nothing ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        ]
                        []
    , text =
        Html.text
    , unorderedList =
        \items ->
            Html.ul []
                (items
                    |> List.map
                        (\item ->
                            case item of
                                Block.ListItem task children ->
                                    let
                                        checkbox =
                                            case task of
                                                Block.NoTask ->
                                                    Html.text ""

                                                Block.IncompleteTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked False
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []

                                                Block.CompletedTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked True
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []
                                    in
                                    Html.li [] (checkbox :: children)
                        )
                )
    , orderedList =
        \startingIndex items ->
            Html.ol
                (case startingIndex of
                    1 ->
                        [ Attr.start startingIndex ]

                    _ ->
                        []
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            Html.li []
                                itemBlocks
                        )
                )
    , html = Markdown.Html.oneOf []
    , codeBlock =
        \{ body, language } ->
            let
                classes =
                    -- Only the first word is used in the class
                    case Maybe.map String.words language of
                        Just (actualLanguage :: _) ->
                            [ Attr.class <| "language-" ++ actualLanguage ]

                        _ ->
                            []
            in
            Html.pre []
                [ Html.code classes
                    [ Html.text body
                    ]
                ]
    , thematicBreak = Html.hr [] []
    , table = Html.table []
    , tableHeader = Html.thead []
    , tableBody = Html.tbody []
    , tableRow = Html.tr []
    , tableHeaderCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.th attrs
    , tableCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.td attrs
    , strikethrough =
        \children -> Html.del [] children
    }

```

### Rendering

There are two ways to turn parsed markdown blocks into your rendered view:

- **`Markdown.Renderer.render`** returns `List view` directly — rendering cannot fail. This is the recommended default. It requires a `Renderer Never view`, which you get by using `Markdown.Html.withFallback` on your HTML renderer.
- **`Markdown.Renderer.tryRender`** returns `Result String (List view)` — rendering can fail if it encounters HTML tags that aren't handled by your `Markdown.Html.oneOf` list. Use this when you want explicit errors for unexpected HTML.

To use `render`, convert your fallible HTML renderer into an infallible one with `withFallback`:

```elm
import Markdown.Html

htmlRenderer : Markdown.Html.Renderer Never (List (Html msg) -> Html msg)
htmlRenderer =
    Markdown.Html.oneOf
        [ Markdown.Html.tag "bio"
            (\name children -> bioView name children)
            |> Markdown.Html.withAttribute "name"
        ]
        |> Markdown.Html.withFallback
            (\tagName attributes { rendered } ->
                Html.node tagName
                    (List.map (\{ name, value } -> Html.Attributes.attribute name value) attributes)
                    rendered
            )
```

The fallback function handles any tags not matched by your `oneOf` list. It receives the tag name, attributes, and a record with `raw` (unparsed source) and `rendered` (the rendered children). A common pattern is to pass them through as native HTML nodes with `Html.node`.

### Markdown Block Transformation

You get full access to the parsed markdown blocks before passing it to a renderer. That means that you can inspect it, do custom logic on it, perform validations, or even go in and transform it! It's totally customizable, and of course it's all just nice Elm custom types!

[Here's a live Ellie example](https://ellie-app.com/6QtYW8pcCDna1) that transforms the AST into a table of contents and renders a `TOC` data type along with the rendered markdown.

## Philosophy & Goals

- Render markdown to any type (`Html`, `elm-ui` `Element`s, `String`s representing ANSI color codes for terminal output... or even a function, allowing you to inject dynamic values into your markdown view)
- Extend markdown without adding to the syntax using custom HTML renderers, with `Markdown.Html.withFallback` to handle unexpected tags gracefully or `Markdown.Html.oneOf` to fail explicitly for unregistered tags
- Provide an infallible `render` pipeline by default, with `tryRender` available when you want custom rendering failures with nice error messages (for example, broken links, or custom validation like titles that are too long)

### Parsing Goals

- Do not add any new syntax, this library has a subset of the features of Github flavored markdown.
- Only parse the [Github-flavored markdown style](https://github.github.com/gfm/) (not CommonMark or other variants)
- Parsing always succeeds — any input produces some output, consistent with how mature markdown parsers handle arbitrary text. Malformed syntax is treated as plain text.
- Only deviate from Github-flavored markdown rules in cases that improve the user experience. In all other cases, follow the Github-flavored markdown spec.

## Current Github-flavored markdown compliance

The test suite for this library runs through all the expected outputs outlined in the GFM spec. It uses the same test suite to test these cases as highlight.js (the library that `elm-explorations/elm-markdown` uses under the hood).

You can see the latest passing and failing tests from this test suite in the `test-results` folder [(in particular, take a look at the Github-Flavored Markdown failures in `failing/GFM`](https://github.com/dillonkearns/elm-markdown/tree/master/test-results/failing/GFM).

## Contributors

A **huge** thanks to [Pablo Hirafuji](https://github.com/pablohirafuji/), who was kind enough to allow me to use his InlineParser in this project. It turns out that Markdown inline parsing is a very specialized algorithm, and the `elm/parser` library isn't suited to solve that particular problem.

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tr>
    <td align="center"><a href="https://github.com/stephenreddek"><img src="https://avatars1.githubusercontent.com/u/1985939?v=4" width="100px;" alt=""/><br /><sub><b>Stephen Reddekopp</b></sub></a><br /><a href="https://github.com/dillonkearns/elm-markdown/commits?author=stephenreddek" title="Tests">⚠️</a> <a href="https://github.com/dillonkearns/elm-markdown/commits?author=stephenreddek" title="Code">💻</a></td>
    <td align="center"><a href="https://github.com/thomasin"><img src="https://avatars3.githubusercontent.com/u/25238976?v=4" width="100px;" alt=""/><br /><sub><b>thomasin</b></sub></a><br /><a href="https://github.com/dillonkearns/elm-markdown/commits?author=thomasin" title="Tests">⚠️</a> <a href="https://github.com/dillonkearns/elm-markdown/commits?author=thomasin" title="Code">💻</a></td>
    <td align="center"><a href="https://brianginsburg.com/"><img src="https://avatars0.githubusercontent.com/u/7957636?v=4" width="100px;" alt=""/><br /><sub><b>Brian Ginsburg</b></sub></a><br /><a href="https://github.com/dillonkearns/elm-markdown/commits?author=bgins" title="Tests">⚠️</a> <a href="https://github.com/dillonkearns/elm-markdown/commits?author=bgins" title="Code">💻</a></td>
    <td align="center"><a href="https://github.com/matheus23"><img src="https://avatars1.githubusercontent.com/u/1430958?v=4" width="100px;" alt=""/><br /><sub><b>Philipp Krüger</b></sub></a><br /><a href="https://github.com/dillonkearns/elm-markdown/commits?author=matheus23" title="Code">💻</a></td>
    <td align="center"><a href="https://github.com/folkertdev"><img src="https://avatars3.githubusercontent.com/u/7949978?v=4" width="100px;" alt=""/><br /><sub><b>Folkert de Vries</b></sub></a><br /><a href="https://github.com/dillonkearns/elm-markdown/commits?author=folkertdev" title="Code">💻</a></td>
  </tr>
</table>

<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

Thank you [@jinjor](https://github.com/jinjor) for your
[`elm-xml-parser`](https://package.elm-lang.org/packages/jinjor/elm-xml-parser/latest/XmlParser) package!

I needed to tweak it so I copied it into the project, but it is one of the dependencies and it worked
without a hitch!
