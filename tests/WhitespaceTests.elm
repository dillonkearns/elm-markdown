module WhitespaceTests exposing (suite)

import Expect
import Helpers
import Parser
import Parser.Advanced as Advanced exposing (..)
import Test exposing (..)
import Whitespace


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
                """chomp
more"""
                    |> Advanced.run
                        (succeed ()
                            |. Helpers.chompUntilLineEndOrEnd
                            |. Whitespace.lineEnd
                            |> Advanced.getChompedString
                        )
                    |> Expect.equal (Ok "chomp\n")
        , test "single carriage return" <|
            \() ->
                """chomp\u{000D}more"""
                    |> Advanced.run
                        (succeed ()
                            |. Helpers.chompUntilLineEndOrEnd
                            |. Whitespace.lineEnd
                            |> Advanced.getChompedString
                        )
                    |> Expect.equal (Ok "chomp\u{000D}")
        , test "carriage return followed by newline" <|
            \() ->
                """chomp\u{000D}
more"""
                    |> Advanced.run
                        (succeed ()
                            |. Helpers.chompUntilLineEndOrEnd
                            |. Whitespace.lineEnd
                            |> Advanced.getChompedString
                        )
                    |> Expect.equal (Ok "chomp\u{000D}\n")
        ]
