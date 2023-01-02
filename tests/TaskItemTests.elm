module TaskItemTests exposing (suite)

import Expect
import Markdown.ListItem exposing (Completion(..), ListItem(..))
import Parser.Advanced as Advanced
import Test exposing (..)


suite : Test
suite =
    describe "list parsing"
        [ test "incomplete task" <|
            \() ->
                "[ ] Task item"
                    |> Advanced.run Markdown.ListItem.parser
                    |> Expect.equal
                        (Ok (TaskItem Incomplete "Task item"))
        , test "complete task" <|
            \() ->
                "[x] Another task item"
                    |> Advanced.run Markdown.ListItem.parser
                    |> Expect.equal
                        (Ok (TaskItem Complete "Another task item"))
        , test "capital X complete task" <|
            \() ->
                "[X] Another task item"
                    |> Advanced.run Markdown.ListItem.parser
                    |> Expect.equal
                        (Ok (TaskItem Complete "Another task item"))
        , test "plain item because space after checkbox is missing" <|
            \() ->
                "[X]Another task item"
                    |> Advanced.run Markdown.ListItem.parser
                    |> Expect.equal
                        (Ok (PlainItem "[X]Another task item"))
        , test "plain item" <|
            \() ->
                "Plain item"
                    |> Advanced.run Markdown.ListItem.parser
                    |> Expect.equal
                        (Ok (PlainItem "Plain item"))
        ]
