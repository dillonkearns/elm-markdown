module ThematicBreakTests exposing (suite)

import Expect
import Parser.Advanced as Advanced
import Test exposing (..)
import ThematicBreak exposing (ThematicBreak(..), parser)


suite : Test
suite =
    describe "thematic break"
        [ test "simple case" <|
            \() ->
                "---"
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok ThematicBreak)
        , test "asterisks" <|
            \() ->
                "***"
                    |> Advanced.run parser
                    |> Expect.equal (Ok ThematicBreak)
        , test "underscores" <|
            \() ->
                "___"
                    |> Advanced.run parser
                    |> Expect.equal (Ok ThematicBreak)
        , test "not enough characters" <|
            \() ->
                "**"
                    |> expectFail
        , test "no thematic break characters fails" <|
            \() ->
                "hello!"
                    |> expectFail
        , test "can start with a space" <|
            \() ->
                " ***"
                    |> Advanced.run parser
                    |> Expect.equal (Ok ThematicBreak)
        , test "can start with up to 3 spaces" <|
            \() ->
                "   ***"
                    |> Advanced.run parser
                    |> Expect.equal (Ok ThematicBreak)
        , test "any number of spaces can occur between and after thematic break tokens" <|
            \() ->
                "- - - -    "
                    |> Advanced.run parser
                    |> Expect.equal (Ok ThematicBreak)
        , test "mixed is not a thematic break" <|
            \() ->
                "--*"
                    |> expectFail
        ]


expectFail : String -> Expect.Expectation
expectFail input =
    case Advanced.run ThematicBreak.parser input of
        Ok _ ->
            Expect.fail "Expected a parser error."

        Err _ ->
            Expect.pass
