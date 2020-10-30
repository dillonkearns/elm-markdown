module InlineTests exposing (suite)

import Dict
import Expect exposing (Expectation)
import HtmlParser
import Markdown.Inline as Inlines
import Markdown.InlineParser
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
        , test "simple link with a full url" <|
            \() ->
                """[Contact](https://example.com/contact)"""
                    |> expectInlines
                        [ Inlines.Link "https://example.com/contact" Nothing [ Inlines.Text "Contact" ] ]
        , test "multiple simple links with full urls" <|
            \() ->
                """[One](https://example.com/1) [Two](https://example.com/2) [Three](https://example.com/3)"""
                    |> expectInlines
                        [ Inlines.Link "https://example.com/1" Nothing [ Inlines.Text "One" ]
                        , Inlines.Text " "
                        , Inlines.Link "https://example.com/2" Nothing [ Inlines.Text "Two" ]
                        , Inlines.Text " "
                        , Inlines.Link "https://example.com/3" Nothing [ Inlines.Text "Three" ]
                        ]
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
        , describe "GFM extended autolinks"
            [ describe "extended www autolinks"
                [ test "basic www autolink" <|
                    \() ->
                        "www.bar.baz\n"
                            |> expectInlines
                                [ Inlines.Link "http://www.bar.baz" Nothing [ Inlines.Text "www.bar.baz" ] ]
                , test "autolink with simple path" <|
                    \() ->
                        "www.bar.baz/help\n"
                            |> expectInlines
                                [ Inlines.Link "http://www.bar.baz/help" Nothing [ Inlines.Text "www.bar.baz/help" ] ]
                , test "autolink inside text" <|
                    \() ->
                        "visit www.bar.baz/help for more info\n"
                            |> expectInlines
                                [ Inlines.Text "visit ", Inlines.Link "http://www.bar.baz/help" Nothing [ Inlines.Text "www.bar.baz/help" ], Inlines.Text " for more info" ]
                ]
            , describe "extended autolink path validation"
                [ test "with multiple trailing punctuation" <|
                    \() ->
                        "www.bar.baz/help?.~\n"
                            |> expectInlines
                                [ Inlines.Link "http://www.bar.baz/help" Nothing [ Inlines.Text "www.bar.baz/help" ], Inlines.Text "?.~" ]
                , test "with multiple embedded punctuation" <|
                    \() ->
                        "www.bar.baz/help?test=a.b?.~\n"
                            |> expectInlines
                                [ Inlines.Link "http://www.bar.baz/help?test=a.b" Nothing [ Inlines.Text "www.bar.baz/help?test=a.b" ], Inlines.Text "?.~" ]
                , test "with trailing unmatched" <|
                    \() ->
                        "www.bar.baz/help?test=a.b)\n"
                            |> expectInlines
                                [ Inlines.Link "http://www.bar.baz/help?test=a.b" Nothing [ Inlines.Text "www.bar.baz/help?test=a.b" ], Inlines.Text ")" ]
                , test "with multiple trailing unmatched" <|
                    \() ->
                        "www.bar.baz/help?test=a.b))\n"
                            |> expectInlines
                                [ Inlines.Link "http://www.bar.baz/help?test=a.b" Nothing [ Inlines.Text "www.bar.baz/help?test=a.b" ], Inlines.Text "))" ]
                , test "with trailing matched" <|
                    \() ->
                        "www.bar.baz/help?test=(a.b)\n"
                            |> expectInlines
                                [ Inlines.Link "http://www.bar.baz/help?test=(a.b)" Nothing [ Inlines.Text "www.bar.baz/help?test=(a.b)" ] ]
                , test "with embedded unmatched" <|
                    \() ->
                        "www.bar.baz/help?test=(a.b))+ok\n"
                            |> expectInlines
                                [ Inlines.Link "http://www.bar.baz/help?test=(a.b))+ok" Nothing [ Inlines.Text "www.bar.baz/help?test=(a.b))+ok" ] ]
                , test "with trailing entity reference" <|
                    \() ->
                        "www.bar.baz/help?test=a&hl;\n"
                            |> expectInlines
                                [ Inlines.Link "http://www.bar.baz/help?test=a" Nothing [ Inlines.Text "www.bar.baz/help?test=a" ], Inlines.Text "&hl;" ]
                , test "with multiple trailing entity references" <|
                    \() ->
                        "www.bar.baz/help?test=a&hl;&hl;\n"
                            |> expectInlines
                                [ Inlines.Link "http://www.bar.baz/help?test=a" Nothing [ Inlines.Text "www.bar.baz/help?test=a" ], Inlines.Text "&hl;&hl;" ]
                , test "with embedded entity reference" <|
                    \() ->
                        "www.bar.baz/help?test=&hl;+ok\n"
                            |> expectInlines
                                [ Inlines.Link "http://www.bar.baz/help?test=&hl;+ok" Nothing [ Inlines.Text "www.bar.baz/help?test=&hl;+ok" ] ]
                ]
            , describe "extended url autolinks"
                [ test "basic http url" <|
                    \() ->
                        "http://www.bar.baz\n"
                            |> expectInlines
                                [ Inlines.Link "http://www.bar.baz" Nothing [ Inlines.Text "http://www.bar.baz" ] ]
                , test "basic https url" <|
                    \() ->
                        "https://www.bar.baz\n"
                            |> expectInlines
                                [ Inlines.Link "https://www.bar.baz" Nothing [ Inlines.Text "https://www.bar.baz" ] ]
                , test "url ending in a slash" <|
                    \() ->
                        "https://www.bar.baz/\n"
                            |> expectInlines
                                [ Inlines.Link "https://www.bar.baz/" Nothing [ Inlines.Text "https://www.bar.baz/" ] ]
                , test "url with nested page" <|
                    \() ->
                        "https://www.bar.baz/foo1/foo2\n"
                            |> expectInlines
                                [ Inlines.Link "https://www.bar.baz/foo1/foo2" Nothing [ Inlines.Text "https://www.bar.baz/foo1/foo2" ] ]
                , test "url with complicated path" <|
                    \() ->
                        "(Visit https://encrypted.google.com/search?q=Markup+(business))\n"
                            |> expectInlines
                                [ Inlines.Text "(Visit ", Inlines.Link "https://encrypted.google.com/search?q=Markup+(business)" Nothing [ Inlines.Text "https://encrypted.google.com/search?q=Markup+(business)" ], Inlines.Text ")" ]
                , test "when they're inside an html anchor tag" <|
                    \() ->
                        "Already linked: <a href=\"http://example.com/\">http://example.com/</a>."
                            |> expectInlines
                                [ Inlines.Text "Already linked: ", Inlines.HtmlInline (HtmlParser.Element "a" [ { name = "href", value = "http://example.com/" } ] [ HtmlParser.Text "http://example.com/" ]), Inlines.Text "." ]
                ]
            , describe "extended email autolinks"
                [ test "basic email autolink" <|
                    \() ->
                        "hello+xyz@mail.example"
                            |> expectInlines
                                [ Inlines.Link "mailto:hello+xyz@mail.example" Nothing [ Inlines.Text "hello+xyz@mail.example" ] ]
                , test "email autolinks must have a dot in the domain" <|
                    \() ->
                        "hello+xyz@mail"
                            |> expectInlines
                                [ Inlines.Text "hello+xyz@mail" ]
                , test "email autolinks cannot end in a hyphen" <|
                    \() ->
                        "hello+xyz@mail.example-"
                            |> expectInlines
                                [ Inlines.Text "hello+xyz@mail.example-" ]
                ]
            ]

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
