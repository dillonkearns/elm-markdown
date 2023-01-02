module HelpersTests exposing (suite)

import Expect
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
    describe "chompUntilLineEndOrEnd"
        [ test "successfully parses the end" <|
            \() ->
                """chompmore"""
                    |> Advanced.run
                        (succeed ()
                            |. Helpers.chompUntilLineEndOrEnd
                            |. Helpers.lineEndOrEnd
                            |> Advanced.getChompedString
                        )
                    |> Expect.equal (Ok "chompmore")
        ]
