module HelpersTests exposing (suite)

import Expect
import Helpers
import Parser.Advanced as Advanced exposing (..)
import Test exposing (..)


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
