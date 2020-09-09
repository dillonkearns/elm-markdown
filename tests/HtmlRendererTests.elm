module HtmlRendererTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Block as Block exposing (Block)
import Markdown.Html
import Markdown.Parser as Markdown exposing (..)
import Markdown.Renderer
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
parse =
    Markdown.parse


render renderer markdown =
    markdown
        |> Markdown.parse
        |> Result.mapError deadEndsToString
        |> Result.andThen (\ast -> Markdown.Renderer.render renderer ast)


deadEndsToString deadEnds =
    deadEnds
        |> List.map deadEndToString
        |> String.join "\n"


type Rendered tag
    = Unexpected String
    | Html tag


testRenderer : List (Markdown.Html.Renderer (List (Rendered a) -> Rendered a)) -> Markdown.Renderer.Renderer (Rendered a)
testRenderer htmlRenderer =
    { heading =
        \{ level, children } ->
            Unexpected "heading"
    , paragraph = \_ -> Unexpected "String"
    , blockQuote = \_ -> Unexpected "String"
    , strong =
        \content -> Unexpected "String"
    , emphasis =
        \content -> Unexpected "String"
    , hardLineBreak = Unexpected "String"
    , codeSpan =
        \content -> Unexpected "String"
    , image = \link -> Unexpected "String"
    , link =
        \link content ->
            Unexpected "String"
    , text =
        \_ ->
            Unexpected "String"
    , unorderedList =
        \items ->
            Unexpected "String"
    , orderedList =
        \startingIndex items ->
            Unexpected "String"
    , html = Markdown.Html.oneOf htmlRenderer
    , codeBlock =
        \{ body, language } ->
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
                            [ Markdown.Html.tag "social-links" (\children -> Html "social-links")
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
                                (\buttonText children ->
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
                                (\buttonText children ->
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
                                (\buttonText children ->
                                    Html
                                        { tag = "signup-form"
                                        , buttonText = Just buttonText
                                        }
                                )
                                |> Markdown.Html.withAttribute "button"
                            , Markdown.Html.tag "signup-form"
                                (\children ->
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
                                (\first second children ->
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
                                (\name twitter github children ->
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
                                (\urlText children ->
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
                                (\urlText children ->
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
                                (\urlText children ->
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
