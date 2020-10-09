module WhitespaceTests exposing (suite)

import Expect
import Whitespace
import Helpers
import Parser
import Parser.Advanced as Advanced exposing (..)
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


chomper : Parser () -> Parser String
chomper parser =
    parser |> Advanced.getChompedString


suite : Test
suite =
    describe "lineEnd"
        [ test "single new line" <|
            \() ->
                """chomp\nmore"""
            |> Advanced.run
                ( succeed ()
                    |. Helpers.chompUntilLineEndOrEnd
                    |. Whitespace.lineEnd
                    |> Advanced.getChompedString
                )
            |> Expect.equal (Ok "chomp\n")
        , test "single carriage return" <|
            \() ->
                """chomp\rmore"""
            |> Advanced.run
                ( succeed ()
                    |. Helpers.chompUntilLineEndOrEnd
                    |. Whitespace.lineEnd
                    |> Advanced.getChompedString
                )
            |> Expect.equal (Ok "chomp\r")
        , test "carriage return followed by newline" <|
            \() ->
                """chomp\r\nmore"""
            |> Advanced.run
                ( succeed ()
                    |. Helpers.chompUntilLineEndOrEnd
                    |. Whitespace.lineEnd
                    |> Advanced.getChompedString
                )
            |> Expect.equal (Ok "chomp\r\n")
        ]
