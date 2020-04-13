module NoEmptyDocCommentsTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import NoEmptyDocComments
import Review.Test
import Test exposing (..)


suite : Test
suite =
    describe "NoEmptyDocComments"
        [ test "no errors when there's no doc comment" <|
            \() ->
                """module A exposing (..)

a = foo n
"""
                    |> Review.Test.run NoEmptyDocComments.rule
                    |> Review.Test.expectNoErrors
        ]
