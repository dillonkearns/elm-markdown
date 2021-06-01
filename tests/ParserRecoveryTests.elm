module ParserRecoveryTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz
import Markdown.Block as Block exposing (..)
import Markdown.Inline
import Markdown.Parser as Markdown exposing (..)
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=))
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
parse markdown =
    markdown
        |> Markdown.parse


suite : Test
suite =
    describe "parser errors all have fallbacks so parsing always succeeds"
        [ fuzz Fuzz.string "random strings are parseable" <|
            \randomString ->
                randomString
                    |> Markdown.parse
                    |> Expect.ok
        , test "recover from invalid html" <|
            \() ->
                "<no-closing-tag maybe this can just be rendered as a regular paragraph and not as HTML"
                    |> Markdown.parse
                    |> Expect.equal
                        (Ok
                            [ Block.Paragraph
                                [ Block.Text
                                    "<no-closing-tag maybe this can just be rendered as a regular paragraph and not as HTML"
                                ]
                            ]
                        )
        ]


expectOk : List Block -> String -> Expectation
expectOk expected input =
    case input |> parse of
        Ok actual ->
            actual
                |> Expect.equal expected

        Err error ->
            Expect.fail (Debug.toString error)


plainListItem : String -> Block.ListItem Block.Inline
plainListItem body =
    Block.ListItem Block.NoTask [ Block.Text body ]


unstyledText : String -> List Inline
unstyledText body =
    [ Block.Text body ]


emphasisText : String -> List Inline
emphasisText body =
    [ Block.Emphasis <|
        [ Block.Text body ]
    ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
