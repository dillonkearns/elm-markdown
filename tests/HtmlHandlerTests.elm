module HtmlHandlerTests exposing (suite)

import Expect exposing (Expectation)
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


type Rendered
    = Unexpected String
    | Html String


testRenderer : List (Markdown.Decoder (List Rendered -> Rendered)) -> Markdown.Renderer Rendered
testRenderer htmlHandlers =
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
    , htmlDecoder = Markdown.htmlOneOf htmlHandlers
    , codeBlock =
        \{ body, language } ->
            Unexpected "String"
    , thematicBreak = Unexpected "String"
    }


suite : Test
suite =
    describe "html handlers"
        [ test "basic html tag" <|
            \() ->
                "<social-links />"
                    |> Markdown.render
                        (testRenderer
                            [ Markdown.htmlTag "social-links" (\children -> Html "social-links")
                            ]
                        )
                    |> Expect.equal (Ok [ Html "social-links" ])
        , test "unregistered html tag" <|
            \() ->
                "<social-links />"
                    |> Markdown.render
                        (testRenderer [])
                    |> Expect.equal (Err "Ran into a oneOf with no possibilities!")
        , test "html attribute parser" <|
            \() ->
                """<signup-form button="Sign up now!" />"""
                    |> Markdown.render
                        (testRenderer
                            [ Markdown.htmlTag "signup-form" (\buttonText children -> Html ("signup-form " ++ buttonText))
                                |> Markdown.withAttribute "button"
                            ]
                        )
                    |> Expect.equal (Ok [ Html "signup-form Sign up now!" ])
        , test "fail on missing attribute" <|
            \() ->
                """<signup-form />"""
                    |> Markdown.render
                        (testRenderer
                            [ Markdown.htmlTag "signup-form" (\buttonText children -> Html ("signup-form " ++ buttonText))
                                |> Markdown.withAttribute "button"
                            ]
                        )
                    |> Expect.equal (Err """Problem with the given value:

<signup-form>

Expecting attribute "button".
""")
        , test "give details for all failures in oneOf" <|
            \() ->
                """<unregistered-tag />"""
                    |> Markdown.render
                        (testRenderer
                            [ Markdown.htmlTag "signup-form" (\buttonText children -> Html ("signup-form " ++ buttonText))
                                |> Markdown.withAttribute "button"
                            , Markdown.htmlTag "signup-form" (\children -> Html "signup-form")
                            ]
                        )
                    |> Expect.equal (Err """oneOf failed parsing this value:
    <unregistered-tag>

Parsing failed in the following 2 ways:


(1) Expected signup-form but was unregistered-tag

(2) Expecting attribute "button".
""")
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
