module Tests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Parser exposing (..)
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) Block
parse input =
    Advanced.run lineParser input


suite : Test
suite =
    describe "parsing"
        [ describe "headings"
            [ test "Heading 1" <|
                \() ->
                    "# Hello!"
                        |> parse
                        |> Expect.equal (Ok (Heading 1 "Hello!"))
            , test "Heading 2" <|
                \() ->
                    "## Hello!"
                        |> parse
                        |> Expect.equal (Ok (Heading 2 "Hello!"))
            , skip <|
                -- TODO limit parsing over heading level 7, see https://spec.commonmark.org/0.27/#atx-headings
                test "Heading 7 is invalid"
                <|
                    \() ->
                        "####### Hello!"
                            |> parserError
            ]
        , test "plain text" <|
            \() ->
                "This is just some text"
                    |> parse
                    |> Expect.equal (Ok (Body (unstyledText "This is just some text")))
        , test "parse heading then plain text" <|
            \() ->
                """# Heading
This is just some text
"""
                    |> Advanced.run multiParser
                    |> Expect.equal
                        (Ok
                            [ Heading 1 "Heading"
                            , Body (unstyledText "This is just some text")
                            ]
                        )
        , skip <|
            test "doesn't need to end in newline" <|
                \() ->
                    """# Heading
This is just some text"""
                        |> Advanced.run multiParser
                        |> Expect.equal
                            (Ok
                                [ Heading 1 "Heading"
                                , Body (unstyledText "This is just some text")
                                ]
                            )
        , test "long example" <|
            \() ->
                """# Heading

This is just some text.

## Subheading

Body of the subheading.
"""
                    |> Advanced.run multiParser
                    |> Expect.equal
                        (Ok
                            [ Heading 1 "Heading"
                            , Body (unstyledText "")
                            , Body (unstyledText "This is just some text.")
                            , Body (unstyledText "")
                            , Heading 2 "Subheading"
                            , Body (unstyledText "")
                            , Body (unstyledText "Body of the subheading.")
                            ]
                        )
        , test "embedded HTML" <|
            \() ->
                """# Heading
<div>
Hello!
</div>
"""
                    |> Advanced.run multiParser
                    |> Expect.equal
                        (Ok
                            [ Heading 1 "Heading"
                            , Html "div"
                                []
                                [ -- TODO how should `Body ""` be handled?
                                  Body (unstyledText "")
                                , Body (unstyledText "Hello!")
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
                    |> Advanced.run multiParser
                    |> Expect.equal
                        (Ok
                            [ Heading 1 "Heading"
                            , Html "div"
                                []
                                [ Body (unstyledText "")
                                , Heading 1 "Heading in a div!"
                                , Body (unstyledText "")
                                ]
                            ]
                        )
        ]


unstyledText body =
    [ { string = body, style = { isBold = False, isItalic = False } } ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
