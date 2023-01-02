module HtmlRendererTests exposing (suite)

import Expect
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer
import Parser
import Parser.Advanced
import Test exposing (..)


render : Markdown.Renderer.Renderer view -> String -> Result String (List view)
render renderer markdown =
    markdown
        |> Markdown.parse
        |> Result.mapError deadEndsToString
        |> Result.andThen (\ast -> Markdown.Renderer.render renderer ast)


deadEndsToString : List (Parser.Advanced.DeadEnd String Parser.Problem) -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"


type Rendered tag
    = Unexpected String
    | Html tag


testRenderer : List (Markdown.Html.Renderer (List (Rendered a) -> Rendered a)) -> Markdown.Renderer.Renderer (Rendered a)
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
        ]
