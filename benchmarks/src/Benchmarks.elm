module Benchmarks exposing (suite)

import Benchmark exposing (Benchmark, describe)
import Markdown
import Markdown.Parser


suite : Benchmark
suite =
    describe "markdown parsing"
        [ "# This is a heading"
            |> compare "just a heading"
        , elmMarkdownExplorationsReadme
            |> compare "elm-explorations/markdown readme"
        , withHeadingsAndLists
            |> compare "withHeadingsAndLists"
        ]


elmMarkdownExplorationsReadme =
    """# elm-markdown

## Level 2 heading

### Level 3 heading

"""


withHeadingsAndLists =
    """# elm-markdown

- Item 1 
- Item 2 
- Item 3 

## Level 2 heading

- [Google](https://google.com)
- [Bing](https://bing.com)
- [DuckDuckGo](https://duckduckgo.com)

### Level 3 heading

- Item 1
- Item 2
- Item 3
"""


compare title markdown =
    Benchmark.benchmark title
        (\_ -> Markdown.Parser.parse markdown)



--Benchmark.compare "long example"
--    "elm-markdown-decoder"
--    (\_ -> Markdown.Parser.parse markdown)
--    "elm-explorations/markdown"
--    (\_ -> explorationsParse markdown)


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
