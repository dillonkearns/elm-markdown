module Tests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Block as Block exposing (Block)
import Markdown.Inlines
import Markdown.Parser exposing (..)
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
parse =
    Markdown.Parser.parse


suite : Test
suite =
    describe "parsing"
        [ describe "headings"
            [ test "Heading 1" <|
                \() ->
                    "# Hello!"
                        |> parse
                        |> Expect.equal (Ok [ Block.Heading 1 (unstyledText "Hello!") ])
            , test "heading can end with trailing #'s'" <|
                \() ->
                    "# Hello! ###"
                        |> parse
                        |> Expect.equal (Ok [ Block.Heading 1 (unstyledText "Hello!") ])
            , test "Heading 2" <|
                \() ->
                    "## Hello!"
                        |> parse
                        |> Expect.equal (Ok [ Block.Heading 2 (unstyledText "Hello!") ])
            , test "Heading 7 is invalid" <|
                \() ->
                    "####### Hello!"
                        |> parserError
            ]
        , test "plain text" <|
            \() ->
                "This is just some text"
                    |> parse
                    |> Expect.equal (Ok [ Block.Body (unstyledText "This is just some text") ])
        , test "parse heading then plain text" <|
            \() ->
                """# Heading
This is just some text
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading 1 (unstyledText "Heading")
                            , Block.Body (unstyledText "This is just some text")
                            ]
                        )
        , test "doesn't need to end in newline" <|
            \() ->
                """# Heading
This is just some text"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading 1 (unstyledText "Heading")
                            , Block.Body (unstyledText "This is just some text")
                            ]
                        )
        , test "long example" <|
            \() ->
                """# Heading

This is just some text.

## Subheading

Body of the subheading.
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading 1 (unstyledText "Heading")
                            , Block.Body (unstyledText "This is just some text.")
                            , Block.Heading 2 (unstyledText "Subheading")
                            , Block.Body (unstyledText "Body of the subheading.")
                            ]
                        )
        , test "embedded HTML" <|
            \() ->
                """# Heading
<div>
Hello!
</div>
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading 1 (unstyledText "Heading")
                            , Block.Html "div"
                                []
                                [ Block.Body (unstyledText "Hello!")
                                ]
                            ]
                        )
        , test "heading within HTML" <|
            \() ->
                """# Heading
<div>
# Heading in a div!

</div>
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading 1 (unstyledText "Heading")
                            , Block.Html "div"
                                []
                                [ Block.Heading 1 (unstyledText "Heading in a div!")
                                ]
                            ]
                        )
        , test "simple list" <|
            \() ->
                """- One
- Two
- Three
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.ListBlock
                                [ unstyledText "One"
                                , unstyledText "Two"
                                , unstyledText "Three"
                                ]

                            -- TODO why is this extra block here? Fix
                            -- , ListBlock []
                            ]
                        )
        , test "thematic break" <|
            \() ->
                """---"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.ThematicBreak
                            ]
                        )
        , test "thematic break followed by newline" <|
            \() ->
                """---
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.ThematicBreak
                            ]
                        )
        , test "mixed content with list" <|
            \() ->
                """# Title

- This is an item
- And so is this

Text after
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading 1 (unstyledText "Title")
                            , Block.ListBlock
                                [ unstyledText "This is an item"
                                , unstyledText "And so is this"
                                ]
                            , Block.Body (unstyledText "Text after")

                            -- TODO why is this extra block here? Fix
                            -- , ListBlock []
                            ]
                        )
        , test "plain text on multiple lines is in one paragraph" <|
            \() ->
                """Line 1
Line 2

Line after blank line"""
                    |> parse
                    |> Expect.equal (Ok [ Block.Body (unstyledText """Line 1
Line 2

Line after blank line""") ])
        , test "code fence with paragraph and heading below" <|
            \() ->
                """```shell
.
├── content/
├── elm.json
├── images/
├── static/
├── index.js
├── package.json
└── src/
    └── Main.elm
```

This is more stuff

## h2

qwer
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.CodeBlock
                                { body = ".\n├── content/\n├── elm.json\n├── images/\n├── static/\n├── index.js\n├── package.json\n└── src/\n    └── Main.elm"
                                , language = Just "shell"
                                }
                            , Block.Body (unstyledText "This is more stuff")
                            , Block.Heading 2 (unstyledText "h2")
                            , Block.Body (unstyledText "qwer")
                            ]
                        )
        , test "indented code block" <|
            \() ->
                """    foo = 123"""
                    |> parse
                    |> Expect.equal (Ok [ Block.CodeBlock { body = "foo = 123", language = Nothing } ])
        , test "indented code block with tab" <|
            \() ->
                """\tfoo = 123"""
                    |> parse
                    |> Expect.equal (Ok [ Block.CodeBlock { body = "foo = 123", language = Nothing } ])
        , test "image" <|
            \() ->
                """![This is an image](/my/image.jpg)"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Body
                                [ { string = "This is an image"
                                  , style =
                                        { isBold = False
                                        , isCode = False
                                        , isItalic = False
                                        , link = Just { destination = Markdown.Inlines.Image "/my/image.jpg", title = Nothing }
                                        }
                                  }
                                ]
                            ]
                        )
        ]


unstyledText : String -> List Markdown.Inlines.StyledString
unstyledText body =
    [ { string = body
      , style =
            { isCode = False
            , isBold = False
            , isItalic = False
            , link = Nothing
            }
      }
    ]


unstyledTextSingle : String -> Markdown.Inlines.StyledString
unstyledTextSingle body =
    { string = body
    , style =
        { isCode = False
        , isBold = False
        , isItalic = False
        , link = Nothing
        }
    }


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
