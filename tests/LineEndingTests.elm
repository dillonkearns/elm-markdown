module LineEndingTests exposing (suite)

import Expect exposing (Expectation)
import LineEnding
import Parser.Advanced as Advanced exposing ((|.), (|=))
import Test exposing (..)


suite : Test
suite =
    describe "line endings"
        [ test "single newline" <|
            \() ->
                "\n"
                    |> expectSuccess
        , test "non-whitespace" <|
            \() ->
                "This is not whitespace"
                    |> expectNothingConsumed
        , test "whitespace without line endings" <|
            \() ->
                "   \t \t"
                    |> expectSuccess
        , test "multiple line endings" <|
            \() ->
                "\n\n"
                    |> expectError
        ]


expectSuccess : String -> Expect.Expectation
expectSuccess input =
    input
        |> Advanced.run LineEnding.optionalWhitespaceUpToOneLineEnding
        |> Expect.equal (Ok ())


expectError : String -> Expectation
expectError input =
    case input |> Advanced.run LineEnding.optionalWhitespaceUpToOneLineEnding of
        Ok value ->
            Expect.fail <| "Expecting an error. Got:\n\n" ++ Debug.toString value

        Err _ ->
            Expect.pass


expectNothingConsumed : String -> Expectation
expectNothingConsumed input =
    case
        input
            |> Advanced.run
                (Advanced.succeed identity
                    |. LineEnding.optionalWhitespaceUpToOneLineEnding
                    |= Advanced.getOffset
                )
    of
        Ok offset ->
            if offset > 0 then
                Expect.fail <| "Expected parser not to consume any characters, but it consumed " ++ String.fromInt offset

            else
                Expect.pass

        Err _ ->
            Expect.fail "Expecting parser to succeed but it failed."
