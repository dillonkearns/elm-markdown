module Example exposing (..)

import Expect exposing (Expectation)
import Parser exposing (..)
import Test exposing (..)


type Block
    = Heading Int String
    | Body String


body : Parser Block
body =
    succeed Body
        |= Parser.getChompedString
            (Parser.succeed ()
                |. Parser.chompWhile (\c -> c /= '\n')
            )


lineParser : Parser Block
lineParser =
    Parser.oneOf
        [ heading
        , body
        ]


multiParser : Parser (List Block)
multiParser =
    loop [] statementsHelp


statementsHelp : List Block -> Parser (Step (List Block) (List Block))
statementsHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= lineParser
            -- |. spaces
            |. symbol "\n"

        -- |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revStmts))
        ]


heading : Parser Block
heading =
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


parse : String -> Result (List Parser.DeadEnd) Block
parse input =
    Parser.run lineParser input


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

            -- TODO limit parsing over heading level 7, see https://spec.commonmark.org/0.27/#atx-headings
            -- , test "Heading 7 is invalid" <|
            --     \() ->
            --         "####### Hello!"
            --             |> parserError
            ]
        , test "plain text" <|
            \() ->
                "This is just some text"
                    |> parse
                    |> Expect.equal (Ok (Body "This is just some text"))
        , test "parse heading then plain text" <|
            \() ->
                """# Heading
This is just some text
"""
                    |> Parser.run multiParser
                    |> Expect.equal
                        (Ok
                            [ Heading 1 "Heading"
                            , Body "This is just some text"
                            ]
                        )
        ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
