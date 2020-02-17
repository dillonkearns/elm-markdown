module InlineTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Block as Block exposing (Block, Inline)
import Markdown.Inlines as Inlines
import Markdown.Parser exposing (..)
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


suite : Test
suite =
    describe "inline parsing"
        [ test "code spans" <|
            \() ->
                """`code`"""
                    |> Advanced.run Inlines.parse
                    |> Expect.equal
                        (Ok [ Block.CodeSpan "code" ])
        , test "code span with double backtick skips over internal single backtick" <|
            \() ->
                """``this can have a ` character inside``"""
                    |> Advanced.run Inlines.parse
                    |> Expect.equal
                        (Ok [ Block.CodeSpan "this can have a ` character inside" ])
        , test "inline code takes precedence over italics" <|
            \() ->
                "`find . -name '*.html'`"
                    |> Advanced.run Inlines.parse
                    |> Expect.equal (Ok [ Block.CodeSpan "find . -name '*.html'" ])
        , test "plain text" <|
            \() ->
                "Nothing interesting here!"
                    |> Advanced.run Inlines.parse
                    |> Expect.equal
                        (Ok
                            [ Block.Text "Nothing interesting here!"
                            ]
                        )
        , test "emphasis parsing" <|
            \() ->
                "*hello!*"
                    |> Advanced.run Inlines.parse
                    |> Expect.equal
                        (Ok
                            [ Block.Italic <|
                                Block.Text "hello!"
                            ]
                        )
        , test "multiple code spans" <|
            \() ->
                """` `
`  `
"""
                    |> Advanced.run Inlines.parse
                    |> Expect.equal
                        (Ok
                            [ Block.CodeSpan " "
                            , Block.CodeSpan "  "
                            ]
                        )
        , test "simple link" <|
            \() ->
                """[Contact](/contact)"""
                    |> Advanced.run Inlines.parse
                    |> Expect.equal
                        (Ok
                            [ Block.Link
                                { href = "/contact"
                                }
                                [ Block.Text "Contact" ]
                            ]
                        )
        , test "link with formatting" <|
            \() ->
                """[This `code` is *really* awesome](/contact)"""
                    |> Advanced.run Inlines.parse
                    |> Expect.equal
                        (Ok
                            [ Block.Link
                                { href = "/contact"
                                }
                                [ Block.Text "This "
                                , Block.CodeSpan "code"
                                , Block.Text " is "
                                , Block.Italic (Block.Text "really")
                                , Block.Text " awesome"
                                ]
                            ]
                        )
        , test "links have precedence over *" <|
            \() ->
                "*foo [bar*](/url)"
                    |> expectInlines
                        [ Block.Text "*foo "
                        , Block.Link { href = "/url" } [ Block.Text "bar*" ]
                        ]
        , test "string ends while expecting closing italic" <|
            \() ->
                "*this is just a literal star because it's unclosed"
                    |> expectInlines
                        [ Block.Text "*this is just a literal star because it's unclosed"
                        ]
        , test "italicized codespan" <|
            \() ->
                "*`italic codespan`*"
                    |> expectInlines
                        [ Block.Italic <| Block.CodeSpan "italic codespan" ]
        , test "unlike GFM and commonmark, elm-markdown parses image alt as raw text" <|
            \() ->
                "![foo ![bar](/url)](/url2)\n"
                    |> expectInlines
                        [ Block.Italic <| Block.Image { src = "/url2", alt = "foo ![bar](/url)" } ]
        , test "backslash escape" <|
            \() ->
                "\\\\"
                    |> expectInlines
                        [ Block.Text "\\"
                        ]
        , test "hard line break" <|
            \() ->
                "foo\\\nbaz"
                    |> expectInlines
                        [ Block.Text "foo"
                        , Block.HardLineBreak
                        , Block.Text "baz"
                        ]
        , test "backslash newline at end is not hard line break" <|
            \() ->
                "foo\\\n"
                    |> expectInlines [ Block.Text "foo\\" ]

        --, only <|
        --    test "unknown" <|
        --        \() ->
        --            "\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\`\\{\\|\\}\\~\n"
        --                |> expectInlines []
        --        , test "heading within HTML" <|
        --            \() ->
        --                """# Heading
        --<div>
        --# Heading in a div!
        --
        --</div>
        --"""
        --                    |> Markdown.Parser.parse
        --                    |> Expect.equal
        --                        (Ok
        --                            [ Block.Heading 1 (unstyledText "Heading")
        --                            , Block.Html "div"
        --                                []
        --                                [ Block.Heading 1 (unstyledText "Heading in a div!")
        --                                ]
        --                            ]
        --                        )
        --        , test "plain text followed by link" <|
        --            \() ->
        --                """This is an intro and [this is a link](/my/page)"""
        --                    |> Markdown.Parser.parse
        --                    |> Expect.equal
        --                        (Ok
        --                            [ Block.Body
        --                                [ { string = "This is an intro and "
        --                                  , style =
        --                                        { isCode = False
        --                                        , isBold = False
        --                                        , isItalic = False
        --                                        , link = Nothing
        --                                        }
        --                                  }
        --                                , { string = "this is a link"
        --                                  , style =
        --                                        { isCode = False
        --                                        , isBold = False
        --                                        , isItalic = False
        --                                        , link =
        --                                            Just
        --                                                { destination = Block.Link "/my/page"
        --                                                , title = Nothing
        --                                                }
        --                                        }
        --                                  }
        --                                ]
        --                            ]
        --                        )
        ]


expectInlines expected input =
    input
        |> Advanced.run Inlines.parse
        |> Expect.equal (Ok expected)



--unstyledText : String -> List Inline
--unstyledText body =
--    [ { string = body
--      , style =
--            { isCode = False
--            , isBold = False
--            , isItalic = False
--            , link = Nothing
--            }
--      }
--    ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
