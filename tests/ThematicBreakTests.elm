module ThematicBreakTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.ListItem exposing (Completion(..), ListItem(..))
import Markdown.Parser
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type ThematicBreak
    = ThematicBreak


parser : Parser ThematicBreak
parser =
    --Advanced.succeed ThematicBreak
    Advanced.loop NoMatchYet statementsHelp


type State
    = Asterisk Int
    | Hyphen Int
    | NoMatchYet



--statementsHelp : Maybe ThematicChar -> Parser (Advanced.Step ThematicChar ThematicBreak)


statementsHelp : State -> Parser (Advanced.Step State ThematicBreak)
statementsHelp revStmts =
    Advanced.succeed (Advanced.Done ThematicBreak)


suite : Test
suite =
    describe "thematic break"
        [ test "simple case" <|
            \() ->
                "---"
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok ThematicBreak)

        --, test "asterisks" <|
        --    \() ->
        --        "***"
        --            |> Advanced.run parser
        --            |> Expect.equal
        --                (Ok ThematicBreak)
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
