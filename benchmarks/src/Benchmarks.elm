module Benchmarks exposing (suite)

import Benchmark exposing (Benchmark, describe)
import Markdown
import Markdown.OrderedList
import Markdown.Parser
import Parser.Advanced as Advanced
import ThematicBreak


suite : Benchmark
suite =
    describe "markdown parsing"
        [ "# This is a heading"
            |> compare "just a heading"
        , elmMarkdownExplorationsReadme
            |> compare "elm-explorations/markdown readme"
        , withHeadingsAndLists
            |> compare "withHeadingsAndLists"
        , withHeadingsAndListsAndHtml
            |> compare "withHeadingsAndListsAndHtml"
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


withHeadingsAndListsAndHtml =
    """# elm-markdown

- Item 1 
- Item 2 
- Item 3 

## Level 2 heading

<SearchEnginesBox>
- [Google](https://google.com)
- [Bing](https://bing.com)
- [DuckDuckGo](https://duckduckgo.com)
</SearchEnginesBox>

<MyCustomHtmlTag></MyCustomHtmlTag>

<Nested>
<Inner>
## This is a sub-heading

**This is bold**
</Inner>
</Nested>

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


thematicBreak =
    Benchmark.benchmark "thematic break"
        (\_ -> Advanced.run ThematicBreak.parser "_    __  ____")


orderedList =
    Benchmark.benchmark "thematic break"
        (\_ ->
            Advanced.run (Markdown.OrderedList.parser Nothing)
                """1. foo bar
            2. stuff stuff
            3. milk, eggs
            """
        )
