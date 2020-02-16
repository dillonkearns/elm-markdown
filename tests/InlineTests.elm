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
                        (Ok [ Block.InlineContent (Block.CodeSpan "code") ])
        , test "code span with double backtick skips over internal single backtick" <|
            \() ->
                """``this can have a ` character inside``"""
                    |> Advanced.run Inlines.parse
                    |> Expect.equal
                        (Ok [ Block.InlineContent (Block.CodeSpan "this can have a ` character inside") ])
        , test "inline code takes precedence over italics" <|
            \() ->
                "`find . -name '*.html'`"
                    |> Advanced.run Inlines.parse
                    |> Expect.equal (Ok [ Block.InlineContent (Block.CodeSpan "find . -name '*.html'") ])
        , test "plain text" <|
            \() ->
                "Nothing interesting here!"
                    |> Advanced.run Inlines.parse
                    |> Expect.equal
                        (Ok
                            [ Block.InlineContent
                                (Block.Text "Nothing interesting here!")
                            ]
                        )
        , test "emphasis parsing" <|
            \() ->
                "*hello!*"
                    |> Advanced.run Inlines.parse
                    |> Expect.equal
                        (Ok
                            [ Block.InlineContent
                                (Block.Italic <|
                                    Block.Text "hello!"
                                )
                            ]
                        )

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
        --        , test "simple link" <|
        --            \() ->
        --                """[Contact](/contact)
        --"""
        --                    |> Markdown.Parser.parse
        --                    |> Expect.equal
        --                        (Ok
        --                            [ Block.Body
        --                                [ { string = "Contact"
        --                                  , style =
        --                                        { isCode = False
        --                                        , isBold = False
        --                                        , isItalic = False
        --                                        , link =
        --                                            Just
        --                                                { destination = Block.Link "/contact"
        --                                                , title = Nothing
        --                                                }
        --                                        }
        --                                  }
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
