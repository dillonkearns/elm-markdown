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
                |> Parser.map (\_ -> 1)
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
    test "Heading 1" <|
        \() ->
            "# Hello!"
                |> parse
                |> Expect.equal (Ok { level = 1, body = "Hello!" })
