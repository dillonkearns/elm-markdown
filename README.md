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
your HTML renderer, so you get all of your rendered lists, code blocks, links, etc. within your tag. You can try a [live Ellie demo of this code snippet](https://ellie-app.com/8kHgbSLfhfha1).

## Live Code Demos

- [Custom HTML Block Rendering (with `elm-ui`)](https://ellie-app.com/d7R3b9FsHfCa1)
- [Extracting a table of contents from the parsed Markdown](https://ellie-app.com/8kQhZhZpjfca1)
- [Running the built-in, standard markdown HTML renderer](https://ellie-app.com/8kQjvHM9hMfa1)
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

defaultHtmlRenderer : Renderer (Html msg)
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
            Html.pre []
                [ Html.code []
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
    , tableCell = Html.td []
    }
```


### Markdown Block Transformation

You get full access to the parsed markdown blocks before passing it to a renderer. That means that you can inspect it, do custom logic on it, perform validations, or even go in and transform it! It's totally customizable, and of course it's all just nice Elm custom types!

[Here's a live Ellie example](https://ellie-app.com/6QtYW8pcCDna1) that transforms the AST into a table of contents and renders a `TOC` data type along with the rendered markdown.

## Philosophy & Goals

- Render markdown to any type (`Html`, `elm-ui` `Element`s, `String`s representing ANSI color codes for terminal output... or even a function, allowing you to inject dynamic values into your markdown view)
- Extend markdown without adding to the syntax using custom HTML renderers, and fail explicitly for unexpected HTML tags, or missing attributes within those tags
- Allow users to give custom parsing failures with nice error messages (for example, broken links, or custom validation like titles that are too long)

### Parsing Goals

This is evolving and I would like input on the direction of parsing. My current thinking is that this library should:

- Do not add any new syntax, this library has a subset of the features of Github flavored markdown.
- Only parse the [Github-flavored markdown style](https://github.github.com/gfm/) (not CommonMark or other variants)
- (This breaks GFM compliance in favor of explicit errors) All markdown is valid in github-flavored markdown and other variants. This library aims to give explicit errors instead of falling back and silently continuing, see example below
- Only deviate from Github-flavored markdown rules when it helps give better error feedback for "things you probably didn't mean to do." In all other cases, follow the Github-flavored markdown spec.

## Current Github-flavored markdown compliance

The test suite for this library runs through all the expected outputs outlined in the GFM spec. It uses the same test suite to test these cases as highlight.js (the library that `elm-explorations/elm-markdown` uses under the hood).

You can see the latest passing and failing tests from this test suite in the `test-results` folder [(in particular, take a look at the Github-Flavored Markdown failures in in `failing/GFM`](https://github.com/dillonkearns/elm-markdown/tree/master/test-results/failing/GFM).

### Examples of fallback behavior

Github flavored markdown behavior:
Links with missing closing parens are are rendered as raw text instead of links

```markdown
[My link](/home/ wait I forgot to close the link
```

Renders the raw string instead of a link, like so:

```html
<p>
  [My link](/home/ wait I forgot to close the link
</p>
```

This library gives an error message here, and aims to do so in similar situations.

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
