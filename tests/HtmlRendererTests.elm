module HtmlRendererTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Block as Block exposing (Block)
import Markdown.Html
import Markdown.Inlines
import Markdown.Parser as Markdown exposing (..)
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
        |> Result.andThen (\ast -> Markdown.render renderer ast)


deadEndsToString deadEnds =
    deadEnds
        |> List.map deadEndToString
        |> String.join "\n"


type Rendered
    = Unexpected String
    | Html String


testRenderer : List (Markdown.Html.Renderer (List Rendered -> Rendered)) -> Markdown.Renderer Rendered
testRenderer htmlRenderer =
    { heading =
        \{ level, children } ->
            Unexpected "heading"
    , raw = \_ -> Unexpected "String"
    , bold =
        \content -> Unexpected "String"
    , italic =
        \content -> Unexpected "String"
    , code =
        \content -> Unexpected "String"
    , image =
        \link content ->
            Unexpected "String"
                |> Ok
    , link =
        \link content ->
            Unexpected "String"
                |> Ok
    , plain =
        \_ ->
            Unexpected "String"
    , list =
        \items ->
            Unexpected "String"
    , html = Markdown.Html.oneOf htmlRenderer
    , codeBlock =
        \{ body, language } ->
            Unexpected "String"
    , thematicBreak = Unexpected "String"
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
                            [ Markdown.Html.tag "signup-form" (\buttonText children -> Html ("signup-form " ++ buttonText))
                                |> Markdown.Html.withAttribute "button"
                            ]
                        )
                    |> Expect.equal (Ok [ Html "signup-form Sign up now!" ])
        , test "fail on missing attribute" <|
            \() ->
                """<signup-form />"""
                    |> render
                        (testRenderer
                            [ Markdown.Html.tag "signup-form" (\buttonText children -> Html ("signup-form " ++ buttonText))
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
                            [ Markdown.Html.tag "signup-form" (\buttonText children -> Html ("signup-form " ++ buttonText))
                                |> Markdown.Html.withAttribute "button"
                            , Markdown.Html.tag "signup-form" (\children -> Html "signup-form")
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
                            [ Markdown.Html.tag "requires-attribute" (\first second children -> Html ("requires-attribute " ++ first ++ second))
                                |> Markdown.Html.withAttribute "first"
                                |> Markdown.Html.withAttribute "second"
                            ]
                        )
                    |> Expect.equal (Err """Problem with the given value:

<requires-attribute>

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
                                        ("bio "
                                            ++ name
                                            ++ (twitter |> Maybe.withDefault "Nothing")
                                            ++ (github |> Maybe.withDefault "Nothing")
                                        )
                                )
                                |> Markdown.Html.withAttribute "name"
                                |> Markdown.Html.withOptionalAttribute "twitter"
                                |> Markdown.Html.withOptionalAttribute "github"
                            ]
                        )
                    |> Expect.equal (Ok [ Html "bio DillondillontkearnsNothing" ])
        ]
