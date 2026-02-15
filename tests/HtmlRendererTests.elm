module HtmlRendererTests exposing (suite)

import Expect
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer
import Test exposing (..)


render : Markdown.Renderer.Renderer String view -> String -> Result String (List view)
render renderer markdown =
    markdown
        |> Markdown.parse
        |> (\ast -> Markdown.Renderer.tryRender renderer ast)


type Rendered tag
    = Unexpected String
    | Html tag


stringTestRenderer : Markdown.Html.Renderer err (List String -> String) -> Markdown.Renderer.Renderer err String
stringTestRenderer htmlRenderer =
    { heading = \{ children } -> String.join "" children
    , paragraph = String.join ""
    , blockQuote = String.join ""
    , strong = String.join ""
    , emphasis = String.join ""
    , strikethrough = String.join ""
    , hardLineBreak = "\n"
    , codeSpan = identity
    , image = \_ -> ""
    , link = \_ children -> String.join "" children
    , text = identity
    , unorderedList = \_ -> ""
    , orderedList = \_ _ -> ""
    , html = htmlRenderer
    , codeBlock = \{ body } -> body
    , thematicBreak = ""
    , table = \_ -> ""
    , tableHeader = \_ -> ""
    , tableBody = \_ -> ""
    , tableRow = \_ -> ""
    , tableHeaderCell = \_ _ -> ""
    , tableCell = \_ _ -> ""
    }


renderInfallible : Markdown.Renderer.Renderer Never String -> String -> List String
renderInfallible renderer markdown =
    markdown
        |> Markdown.parse
        |> (\ast -> Markdown.Renderer.render renderer ast)


testRenderer : List (Markdown.Html.Renderer String (List (Rendered a) -> Rendered a)) -> Markdown.Renderer.Renderer String (Rendered a)
testRenderer htmlRenderer =
    { heading =
        \_ ->
            Unexpected "heading"
    , paragraph = \_ -> Unexpected "String"
    , blockQuote = \_ -> Unexpected "String"
    , strong =
        \_ -> Unexpected "String"
    , emphasis =
        \_ -> Unexpected "String"
    , strikethrough =
        \_ -> Unexpected "String"
    , hardLineBreak = Unexpected "String"
    , codeSpan =
        \_ -> Unexpected "String"
    , image = \_ -> Unexpected "String"
    , link =
        \_ _ ->
            Unexpected "String"
    , text =
        \_ ->
            Unexpected "String"
    , unorderedList =
        \_ ->
            Unexpected "String"
    , orderedList =
        \_ _ ->
            Unexpected "String"
    , html = Markdown.Html.oneOf htmlRenderer
    , codeBlock =
        \_ ->
            Unexpected "String"
    , thematicBreak = Unexpected "String"
    , table = \_ -> Unexpected "String"
    , tableHeader = \_ -> Unexpected "String"
    , tableBody = \_ -> Unexpected "String"
    , tableRow = \_ -> Unexpected "String"
    , tableHeaderCell = \_ _ -> Unexpected "String"
    , tableCell = \_ _ -> Unexpected "String"
    }


suite : Test
suite =
    describe "html renderers"
        [ test "basic html tag" <|
            \() ->
                "<social-links />"
                    |> render
                        (testRenderer
                            [ Markdown.Html.tag "social-links" (\_ -> Html "social-links")
                            ]
                        )
                    |> Expect.equal (Ok [ Html "social-links" ])
        , test "unregistered html tag" <|
            \() ->
                "<social-links />"
                    |> render
                        (testRenderer [])
                    |> Expect.equal (Err "Ran into a oneOf with no possibilities!")
        , test "html attribute parser" <|
            \() ->
                """<signup-form button="Sign up now!" />"""
                    |> render
                        (testRenderer
                            [ Markdown.Html.tag "signup-form"
                                (\buttonText _ ->
                                    Html
                                        { tag = "signup-form"
                                        , buttonText = buttonText
                                        }
                                )
                                |> Markdown.Html.withAttribute "button"
                            ]
                        )
                    |> Expect.equal
                        (Ok
                            [ Html
                                { tag = "signup-form"
                                , buttonText = "Sign up now!"
                                }
                            ]
                        )
        , test "fail on missing attribute" <|
            \() ->
                """<signup-form />"""
                    |> render
                        (testRenderer
                            [ Markdown.Html.tag "signup-form"
                                (\buttonText _ ->
                                    Html
                                        { tag = "signup-form"
                                        , buttonText = buttonText
                                        }
                                )
                                |> Markdown.Html.withAttribute "button"
                            ]
                        )
                    |> Expect.equal (Err """Problem with the given value:

<signup-form>

Expecting attribute "button".
""")
        , test "give details for all failures in oneOf" <|
            \() ->
                """<unregistered-tag />"""
                    |> render
                        (testRenderer
                            [ Markdown.Html.tag "signup-form"
                                (\buttonText _ ->
                                    Html
                                        { tag = "signup-form"
                                        , buttonText = Just buttonText
                                        }
                                )
                                |> Markdown.Html.withAttribute "button"
                            , Markdown.Html.tag "signup-form"
                                (\_ ->
                                    Html
                                        { tag = "signup-form"
                                        , buttonText = Nothing
                                        }
                                )
                            ]
                        )
                    |> Expect.equal (Err """oneOf failed parsing this value:
    <unregistered-tag>

Parsing failed in the following 2 ways:


(1) Expected signup-form but was unregistered-tag

(2) Expecting attribute "button".
""")
        , test "missing required attribute" <|
            \() ->
                """<requires-attribute second="present" />"""
                    |> render
                        (testRenderer
                            [ Markdown.Html.tag "requires-attribute"
                                (\first second _ ->
                                    Html
                                        { tag = "requires-attribute "
                                        , first = first
                                        , second = second
                                        }
                                )
                                |> Markdown.Html.withAttribute "first"
                                |> Markdown.Html.withAttribute "second"
                            ]
                        )
                    |> Expect.equal (Err """Problem with the given value:

<requires-attribute second="present">

Expecting attribute "first".
""")
        , test "gives back Nothing for missing optional attributes" <|
            \() ->
                """<bio name="Dillon" twitter="dillontkearns" />"""
                    |> render
                        (testRenderer
                            [ Markdown.Html.tag "bio"
                                (\name twitter github _ ->
                                    Html
                                        { tag =
                                            "bio"
                                        , name = name
                                        , twitter = twitter
                                        , github = github
                                        }
                                )
                                |> Markdown.Html.withAttribute "name"
                                |> Markdown.Html.withOptionalAttribute "twitter"
                                |> Markdown.Html.withOptionalAttribute "github"
                            ]
                        )
                    |> Expect.equal
                        (Ok
                            [ Html
                                { tag = "bio"
                                , name = "Dillon"
                                , twitter = Just "dillontkearns"
                                , github = Nothing
                                }
                            ]
                        )
        , test "html entity in attribute" <|
            \() ->
                """<link url="https://example.com?a=1&amp;b=2" />"""
                    |> render
                        (testRenderer
                            [ Markdown.Html.tag "link"
                                (\urlText _ ->
                                    Html
                                        { tag = "link"
                                        , urlText = urlText
                                        }
                                )
                                |> Markdown.Html.withAttribute "url"
                            ]
                        )
                    |> Expect.equal
                        (Ok
                            [ Html
                                { tag = "link"
                                , urlText = "https://example.com?a=1&b=2"
                                }
                            ]
                        )
        , test "html code in attribute" <|
            \() ->
                """<link url="https://example.com?a=1&#38;b=2" />"""
                    |> render
                        (testRenderer
                            [ Markdown.Html.tag "link"
                                (\urlText _ ->
                                    Html
                                        { tag = "link"
                                        , urlText = urlText
                                        }
                                )
                                |> Markdown.Html.withAttribute "url"
                            ]
                        )
                    |> Expect.equal
                        (Ok
                            [ Html
                                { tag = "link"
                                , urlText = "https://example.com?a=1&b=2"
                                }
                            ]
                        )
        , test "hex code in attribute" <|
            \() ->
                """<link url="https://example.com?a=1&#x26;b=2" />"""
                    |> render
                        (testRenderer
                            [ Markdown.Html.tag "link"
                                (\urlText _ ->
                                    Html
                                        { tag = "link"
                                        , urlText = urlText
                                        }
                                )
                                |> Markdown.Html.withAttribute "url"
                            ]
                        )
                    |> Expect.equal
                        (Ok
                            [ Html
                                { tag = "link"
                                , urlText = "https://example.com?a=1&b=2"
                                }
                            ]
                        )
        , describe "withFallback"
            [ test "allows any tag through fallback" <|
                \() ->
                    "<div />"
                        |> renderInfallible
                            (stringTestRenderer
                                (Markdown.Html.oneOf []
                                    |> Markdown.Html.withFallback
                                        (\tag _ _ ->
                                            "<" ++ tag ++ ">"
                                        )
                                )
                            )
                        |> Expect.equal [ "<div>" ]
            , test "specific renderer takes priority over fallback" <|
                \() ->
                    "<custom-tag />"
                        |> renderInfallible
                            (stringTestRenderer
                                (Markdown.Html.oneOf
                                    [ Markdown.Html.tag "custom-tag" (\_ -> "specific-renderer")
                                    ]
                                    |> Markdown.Html.withFallback
                                        (\tag _ _ ->
                                            "fallback-" ++ tag
                                        )
                                )
                            )
                        |> Expect.equal [ "specific-renderer" ]
            , test "fallback receives tag name and attributes" <|
                \() ->
                    """<div class="wrapper" />"""
                        |> renderInfallible
                            (stringTestRenderer
                                (Markdown.Html.oneOf []
                                    |> Markdown.Html.withFallback
                                        (\tag attributes _ ->
                                            let
                                                attrStr : String
                                                attrStr =
                                                    attributes
                                                        |> List.map (\a -> a.name ++ "=" ++ a.value)
                                                        |> String.join ","
                                            in
                                            tag ++ "[" ++ attrStr ++ "]"
                                        )
                                )
                            )
                        |> Expect.equal [ "div[class=wrapper]" ]
            , test "fallback with children content" <|
                \() ->
                    "<div>\n\nhello\n\n</div>"
                        |> renderInfallible
                            (stringTestRenderer
                                (Markdown.Html.oneOf []
                                    |> Markdown.Html.withFallback
                                        (\tag _ { rendered } ->
                                            "<" ++ tag ++ ">" ++ String.join "" rendered ++ "</" ++ tag ++ ">"
                                        )
                                )
                            )
                        |> Expect.equal [ "<div>hello</" ++ "div>" ]
            , test "fallback can escape tags as visible text" <|
                \() ->
                    "<script>\n\nalert('xss')\n\n</script>"
                        |> renderInfallible
                            (stringTestRenderer
                                (Markdown.Html.oneOf []
                                    |> Markdown.Html.withFallback
                                        (\tag _ { rendered } ->
                                            "<" ++ tag ++ ">" ++ String.join "" rendered ++ "</" ++ tag ++ ">"
                                        )
                                )
                            )
                        |> Expect.equal [ "<script>alert('xss')</script>" ]
            , test "fallback can selectively deny tags" <|
                \() ->
                    "<div>\n\n<script>\n\nalert('xss')\n\n</script>\n\n</div>"
                        |> renderInfallible
                            (stringTestRenderer
                                (Markdown.Html.oneOf []
                                    |> Markdown.Html.withFallback
                                        (\tag _ { rendered } ->
                                            if List.member tag [ "script", "iframe", "style" ] then
                                                ""

                                            else
                                                "<" ++ tag ++ ">" ++ String.join "" rendered ++ "</" ++ tag ++ ">"
                                        )
                                )
                            )
                        |> Expect.equal [ "<div></div>" ]
            , test "returns List view directly (no Result)" <|
                \() ->
                    "hello world"
                        |> renderInfallible
                            (stringTestRenderer
                                (Markdown.Html.oneOf []
                                    |> Markdown.Html.withFallback
                                        (\tag _ _ ->
                                            "<" ++ tag ++ ">"
                                        )
                                )
                            )
                        |> Expect.equal [ "hello world" ]
            , test "fallback raw body contains unparsed source text" <|
                \() ->
                    "<style>\n\np {color:red;}\n\n</style>"
                        |> renderInfallible
                            (stringTestRenderer
                                (Markdown.Html.oneOf []
                                    |> Markdown.Html.withFallback
                                        (\tag _ { raw } ->
                                            "<" ++ tag ++ ">" ++ raw ++ "</" ++ tag ++ ">"
                                        )
                                )
                            )
                        |> Expect.equal [ "<style>\n\np {color:red;}\n\n</style>" ]
            , test "fallback raw body preserves unprocessed markdown" <|
                \() ->
                    "<textarea>\n\n*foo*\n\n_bar_\n\n</textarea>"
                        |> renderInfallible
                            (stringTestRenderer
                                (Markdown.Html.oneOf []
                                    |> Markdown.Html.withFallback
                                        (\tag _ { raw } ->
                                            "<" ++ tag ++ ">" ++ raw ++ "</" ++ tag ++ ">"
                                        )
                                )
                            )
                        |> Expect.equal [ "<textarea>\n\n*foo*\n\n_bar_\n\n</textarea>" ]
            ]
        , describe "withRawContent"
            [ test "extracts raw body of a tag" <|
                \() ->
                    "<style>\n\np { color: red; }\n\n</style>"
                        |> render
                            (testRenderer
                                [ Markdown.Html.tag "style"
                                    (\rawBody _ ->
                                        Html
                                            { tag = "style"
                                            , rawBody = rawBody
                                            }
                                    )
                                    |> Markdown.Html.withRawContent
                                ]
                            )
                        |> Expect.equal
                            (Ok
                                [ Html
                                    { tag = "style"
                                    , rawBody = "\n\np { color: red; }\n\n"
                                    }
                                ]
                            )
            , test "withRawContent for self-closing tag has empty body" <|
                \() ->
                    "<my-widget />"
                        |> render
                            (testRenderer
                                [ Markdown.Html.tag "my-widget"
                                    (\rawBody _ ->
                                        Html
                                            { tag = "my-widget"
                                            , rawBody = rawBody
                                            }
                                    )
                                    |> Markdown.Html.withRawContent
                                ]
                            )
                        |> Expect.equal
                            (Ok
                                [ Html
                                    { tag = "my-widget"
                                    , rawBody = ""
                                    }
                                ]
                            )
            , test "withRawContent combined with withAttribute" <|
                \() ->
                    """<code-block language="elm">\n\nmodule Main exposing (..)\n\n</code-block>"""
                        |> render
                            (testRenderer
                                [ Markdown.Html.tag "code-block"
                                    (\language rawBody _ ->
                                        Html
                                            { tag = "code-block"
                                            , language = language
                                            , rawBody = rawBody
                                            }
                                    )
                                    |> Markdown.Html.withAttribute "language"
                                    |> Markdown.Html.withRawContent
                                ]
                            )
                        |> Expect.equal
                            (Ok
                                [ Html
                                    { tag = "code-block"
                                    , language = "elm"
                                    , rawBody = "\n\nmodule Main exposing (..)\n\n"
                                    }
                                ]
                            )
            ]
        , describe "inline html"
            [ test "inline html renders without paragraph wrapping" <|
                \() ->
                    "hello<sup>2</sup>world"
                        |> renderInfallible
                            (stringTestRenderer
                                (Markdown.Html.oneOf []
                                    |> Markdown.Html.withFallback
                                        (\tag _ { rendered } ->
                                            "<" ++ tag ++ ">" ++ String.join "" rendered ++ "</" ++ tag ++ ">"
                                        )
                                )
                            )
                        |> Expect.equal [ "hello<sup>2</sup>world" ]
            ]
        ]
