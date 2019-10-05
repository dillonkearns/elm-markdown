# elm-markdown
[![Build Status](https://travis-ci.org/dillonkearns/elm-markdown.svg?branch=master)](https://travis-ci.org/dillonkearns/elm-markdown)

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
Dillon really likes building things with Elm! Here are some links

- [Articles](https://incrementalelm.com/articles)
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
your HTML renderer, so you get all of your rendered lists, code blocks, links, etc. within your tag.

## Live Code Demos

- [Custom HTML Block Rendering (with `elm-ui`)](https://ellie-app.com/6QtXzdqDdmSa1)
- [Extracting a table of contents from the parsed Markdown](https://ellie-app.com/6QtYW8pcCDna1)
- [Running the built-in, standard markdown HTML renderer](https://ellie-app.com/6Qv2dD7qdBra1)

## Core features

### Custom Renderers

You define your own custom renderer, turning your markdown content into any data type with totally customizable logic. You can even pass back an `Err` to get custom failures (for example, broken links or validations like headings that are too long)!

Here's a snippet from the default HTML renderer that comes built in to give you a sense of how you define a `Renderer`:

```elm
import Html
import Html.Attributes as Attr


defaultHtmlRenderer : Renderer (Html msg)
defaultHtmlRenderer =
    { heading =
        \{ level, children } ->
            case level of
                1 ->
                    Html.h1 [] children

                2 ->
                    Html.h2 [] children

                3 ->
                    Html.h3 [] children

                4 ->
                    Html.h4 [] children

                5 ->
                    Html.h5 [] children

                6 ->
                    Html.h6 [] children

                _ ->
                    Html.text ""
    , raw = Html.p []
    , bold =
        \content -> Html.strong [] [ Html.text content ]
    , italic =
        \content -> Html.em [] [ Html.text content ]
    , code =
        \content -> Html.code [] [ Html.text content ]
    , link =
        \link content ->
            Html.a [ Attr.href link.destination ] content
                |> Ok
    , image =
        \image content ->
            Html.img [ Attr.src image.src ] [ Html.text content ]
                |> Ok
    , -- ...
    }
```

### Custom HTML Blocks

[Check out this example on Ellie](https://ellie-app.com/6QtXzdqDdmSa1) to tweak and take a look at the full code from the top of the README.

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

You can see the latest passing and failing tests from this test suite [in the `test-results` folder](https://github.com/dillonkearns/elm-markdown/tree/master/test-results). Note: because some of the files are very long, Github won't render them correctly (they are in markdown with clickable links). If you want to see a nicer formatted version, you can clone this repo and use a Markdown preview tool in your text editor of choice to open them up.

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

Thank you [@jinjor](https://github.com/jinjor) for your
[`elm-xml-parser`](https://package.elm-lang.org/packages/jinjor/elm-xml-parser/latest/XmlParser) package!

I needed to tweak it so I copied it into the project, but it is one of the dependencies and it worked
without a hitch!
