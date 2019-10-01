module Benchmarks exposing (suite)

import Benchmark exposing (Benchmark, describe)
import Markdown
import Markdown.Parser
import Parser.Advanced as Advanced


suite : Benchmark
suite =
    describe "markdown parsing"
        [ describe "long example"
            [ "# This is a heading"
                |> compare "just a heading"
            , elmMarkdownExplorationsReadme
                |> compare "elm-explorations/markdown readme"
            ]
        ]


elmMarkdownExplorationsReadme =
    """# Markdown in Elm

This package is for markdown parsing and rendering. It is based on the [marked][] project
which focuses on speed.

[marked]: https://github.com/chjj/marked

## Basic Usage

```elm
content : Html msg
content =
Markdown.toHtml [class "content"] \"\"\"

# Apple Pie Recipe

1. Invent the universe.
2. Bake an apple pie.

\"\"\"
```

**Warning:** Calling `Markdown.toHtml` parses the whole block, so try not to
call it for no reason. In the `content` example above we only have to parse
the text once, but if we put it in a function we may be doing a lot of
unnecessary parsing.


## Code Blocks

For highlighting any code blocks, the package relies on the
[highlight.js](https://highlightjs.org/) project. So if you want to
see highlighting of code blocks in the rendering result, you need to
make sure that your page/app binds a version of that library
(supporting the languages you want to handle) to `window.hljs` in
Javascript. [This is how package.elm-lang.org does
that.](https://github.com/elm/package.elm-lang.org/blob/e0b7aa4282038475612722ff7a57195866f8645b/backend/ServeFile.hs#L54)
"""


longExample =
    """# Heading"""


compare title markdown =
    Benchmark.compare "long example"
        "elm-markdown-decoder"
        (\_ -> Markdown.Parser.parse markdown)
        "elm-explorations/markdown"
        (\_ -> explorationsParse markdown)


explorationsParse =
    Markdown.toHtmlWith
        { defaultHighlighting = Just "elm"
        , githubFlavored =
            Just
                { tables = True
                , breaks = True
                }
        , sanitize = False
        , smartypants = True
        }
        []
