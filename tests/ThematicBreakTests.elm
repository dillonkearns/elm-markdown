module ThematicBreakTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.ListItem exposing (Completion(..), ListItem(..))
import Markdown.Parser
import Parser
import Parser.Advanced as Advanced exposing (..)
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type ThematicBreak
    = ThematicBreak


parser : Parser ThematicBreak
parser =
    Advanced.loop NoMatchYet statementsHelp


type State
    = Asterisk Int
    | Hyphen Int
    | NoMatchYet


type ThematicToken
    = Star
    | Dash
    | Finished


statementsHelp : State -> Parser (Advanced.Step State ThematicBreak)
statementsHelp state =
    oneOf
        [ tokenHelp "-" |> map (\_ -> Dash)
        , tokenHelp "*" |> map (\_ -> Star)
        , end (Parser.Expecting "end") |> map (\_ -> Finished)
        , tokenHelp "\n" |> map (\_ -> Finished)
        ]
        |> andThen
            (\thematicToken ->
                case ( thematicToken, state ) of
                    ( Finished, NoMatchYet ) ->
                        problem (Parser.Expecting "TODO")

                    ( Finished, Asterisk occurrences ) ->
                        succeedIfEnough occurrences

                    ( Finished, Hyphen occurrences ) ->
                        succeedIfEnough occurrences

                    ( Star, Asterisk occurrences ) ->
                        Advanced.Loop (Asterisk (occurrences + 1))
                            |> succeed

                    ( Dash, Hyphen occurrences ) ->
                        Advanced.Loop (Hyphen (occurrences + 1))
                            |> succeed

                    ( Star, NoMatchYet ) ->
                        Advanced.Loop (Asterisk 1)
                            |> succeed

                    ( Dash, NoMatchYet ) ->
                        Advanced.Loop (Hyphen 1)
                            |> succeed

                    _ ->
                        problem (Parser.Expecting "TODO")
            )


succeedIfEnough occurences =
    if occurences > 2 then
        succeed (Done ThematicBreak)

    else
        problem (Parser.Expecting "...?")



--|= Advanced.succeed
--    (Advanced.Done ThematicBreak)


tokenHelp char =
    Advanced.token (Advanced.Token char (Parser.Expecting char))


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
                    |> Expect.equal
                        (Ok ThematicBreak)
        , test "not enough characters" <|
            \() ->
                "**"
                    |> expectFail
        , test "no thematic break characters fails" <|
            \() ->
                "hello!"
                    |> expectFail

        --, test "mixed is not a thematic break" <|
        --    \() ->
        --        "--*"
        --            |> Advanced.run parser
        --            |> Expect.equal
        --                (Ok ThematicBreak)
        ]


expectFail input =
    case Advanced.run parser input of
        Ok _ ->
            Expect.fail "Expected a parser error."

        Err _ ->
            Expect.pass
