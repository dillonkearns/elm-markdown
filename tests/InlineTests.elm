module InlineTests exposing (suite)

import Dict
import Expect exposing (Expectation)
import HtmlParser
import Markdown.Inline as Inlines
import Markdown.InlineParser
import Test exposing (..)


suite : Test
suite =
    describe "inline parsing"
        [ test "code spans" <|
            \() ->
                """`code`"""
                    |> expectInlines
                        [ Inlines.CodeInline "code" ]
        , test "code span with double backtick skips over internal single backtick" <|
            \() ->
                """``this can have a ` character inside``"""
                    |> expectInlines
                        [ Inlines.CodeInline "this can have a ` character inside" ]
        , test "inline code takes precedence over italics" <|
            \() ->
                "`find . -name '*.html'`"
                    |> expectInlines [ Inlines.CodeInline "find . -name '*.html'" ]
        , test "plain text" <|
            \() ->
                "Nothing interesting here!"
                    |> expectInlines
                        [ Inlines.Text "Nothing interesting here!"
                        ]
        , test "emphasis parsing" <|
            \() ->
                "*hello!*"
                    |> expectInlines
                        [ Inlines.Emphasis 1 [ Inlines.Text "hello!" ]
                        ]
        , test "No stripping occurs if the code span contains only spaces (example 344)" <|
            \() ->
                """` `
`  `
"""
                    |> expectInlines
                        [ Inlines.CodeInline " "
                        , Inlines.Text "\n"
                        , Inlines.CodeInline "  "
                        ]
        , test "simple link" <|
            \() ->
                """[Contact](/contact)"""
                    |> expectInlines
                        [ Inlines.Link "/contact" Nothing [ Inlines.Text "Contact" ] ]
        , test "link with formatting" <|
            \() ->
                """[This `code` is *really* awesome](/contact)"""
                    |> expectInlines
                        [ Inlines.Link "/contact"
                            Nothing
                            [ Inlines.Text "This "
                            , Inlines.CodeInline "code"
                            , Inlines.Text " is "
                            , Inlines.Emphasis 1 [ Inlines.Text "really" ]
                            , Inlines.Text " awesome"
                            ]
                        ]
        , test "links have precedence over *" <|
            \() ->
                "*foo [bar*](/url)"
                    |> expectInlines
                        [ Inlines.Text "*foo "
                        , Inlines.Link "/url" Nothing [ Inlines.Text "bar*" ]
                        ]
        , test "string ends while expecting closing italic" <|
            \() ->
                "*this is just a literal star because it's unclosed"
                    |> expectInlines
                        [ Inlines.Text "*this is just a literal star because it's unclosed"
                        ]
        , test "italicized codespan" <|
            \() ->
                "*`italic codespan`*"
                    |> expectInlines
                        [ Inlines.Emphasis 1 [ Inlines.CodeInline "italic codespan" ] ]
        , test "autolink" <|
            \() ->
                "<http://foo.bar.baz>\n"
                    |> expectInlines
                        [ Inlines.Link "http://foo.bar.baz" Nothing [ Inlines.Text "http://foo.bar.baz" ] ]

        --, skip <|
        --    test "unlike GFM and commonmark, elm-markdown parses image alt as raw text" <|
        --        \() ->
        --            "![foo ![bar](/url)](/url2)\n"
        --                |> expectInlines
        --                    [ Inlines.Italic <| Inlines.Image { src = "/url2", alt = "foo ![bar](/url)" } ]
        , test "backslash escape" <|
            \() ->
                "\\\\"
                    |> expectInlines
                        [ Inlines.Text "\\"
                        ]
        , test "hard line break" <|
            \() ->
                "foo\\\nbaz"
                    |> expectInlines
                        [ Inlines.Text "foo"
                        , Inlines.HardLineBreak
                        , Inlines.Text "baz"
                        ]
        , test "backslash newline at end is not hard line break" <|
            \() ->
                "foo\\\n"
                    |> expectInlines [ Inlines.Text "foo\\" ]
        , test "escaping italics" <|
            \() ->
                "\\*not emphasized*"
                    |> expectInlines [ Inlines.Text "*not emphasized*" ]
        , describe "html"
            [ test "empty div with closing tag" <|
                \() ->
                    "Hello <div></div> Goodbye"
                        |> expectInlines
                            [ Inlines.Text "Hello "
                            , Inlines.HtmlInline (HtmlParser.Element "div" [] [])
                            , Inlines.Text " Goodbye"
                            ]
            , test "empty div with attributes tag" <|
                \() ->
                    """<div class="foo"></div>"""
                        |> expectInlines
                            [ Inlines.HtmlInline
                                (HtmlParser.Element "div" [ { name = "class", value = "foo" } ] [])
                            ]
            , test "backslash hard line break" <|
                \() ->
                    "This is a hard line break\\\nThis comes after"
                        |> expectInlines
                            [ Inlines.Text "This is a hard line break"
                            , Inlines.HardLineBreak
                            , Inlines.Text "This comes after"
                            ]
            ]

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
        --                            [ Inlines.Heading 1 (unstyledText "Heading")
        --                            , Inlines.Html "div"
        --                                []
        --                                [ Inlines.Heading 1 (unstyledText "Heading in a div!")
        --                                ]
        --                            ]
        --                        )
        --        , test "plain text followed by link" <|
        --            \() ->
        --                """This is an intro and [this is a link](/my/page)"""
        --                    |> Markdown.Parser.parse
        --                    |> Expect.equal
        --                        (Ok
        --                            [ Inlines.Body
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
        --                                                { destination = Inlines.Link "/my/page"
        --                                                , title = Nothing
        --                                                }
        --                                        }
        --                                  }
        --                                ]
        --                            ]
        --                        )
        ]


expectInlines : List Inlines.Inline -> String -> Expectation
expectInlines expected input =
    input
        |> Markdown.InlineParser.parse Dict.empty
        |> Expect.equal expected
