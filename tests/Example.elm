module Example exposing (..)

import Expect exposing (Expectation)
import Parser exposing ((|.), (|=), Parser, float, spaces, succeed, symbol)
import Test exposing (..)


type alias Heading =
    { level : Int
    , body : String
    }


point : Parser Heading
point =
    succeed Heading
        |. symbol "#"
        |= (Parser.getChompedString
                (Parser.succeed ()
                    |. Parser.chompWhile (\c -> c == '#')
                )
                |> Parser.map
                    (\additionalHashes ->
                        String.length additionalHashes + 1
                    )
           )
        |. spaces
        |= Parser.getChompedString
            (Parser.succeed ()
                |. Parser.chompWhile (\c -> c /= '\n')
            )


parse : String -> Result (List Parser.DeadEnd) Heading
parse input =
    Parser.run point input


suite : Test
suite =
    describe "headings"
        [ test "Heading 1" <|
            \() ->
                "# Hello!"
                    |> parse
                    |> Expect.equal (Ok { level = 1, body = "Hello!" })
        , test "Heading 2" <|
            \() ->
                "## Hello!"
                    |> parse
                    |> Expect.equal (Ok { level = 2, body = "Hello!" })

        -- TODO limit parsing over heading level 7, see https://spec.commonmark.org/0.27/#atx-headings
        -- , test "Heading 7 is invalid" <|
        --     \() ->
        --         "####### Hello!"
        --             |> parserError
        ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
