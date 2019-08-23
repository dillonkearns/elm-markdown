module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser exposing ((|.), (|=), Parser, float, spaces, succeed, symbol)
import Test exposing (..)


type alias Point =
    { x : Float
    , y : Float
    }


point : Parser Point
point =
    succeed Point
        |. symbol "("
        |. spaces
        |= float
        |. spaces
        |. symbol ","
        |. spaces
        |= float
        |. spaces
        |. symbol ")"


parse input =
    Parser.run point input


suite : Test
suite =
    test "hello!" <|
        \() ->
            "(1, 2)"
                |> parse
                |> Expect.equal (Ok { x = 1, y = 2 })
