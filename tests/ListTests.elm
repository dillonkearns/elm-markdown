module ListTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.InlineBlock as InlineBlock
import Markdown.Inlines
import Markdown.List
import Markdown.Parser exposing (..)
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


suite : Test
suite =
    describe "list parsing"
        [ test "basic list" <|
            \() ->
                """- Item 1
- Item 2
- Item 3
"""
                    |> Advanced.run Markdown.List.parser
                    |> Expect.equal
                        (Ok
                            [ unstyledText "Item 1"
                            , unstyledText "Item 2"
                            , unstyledText "Item 3"
                            ]
                        )
        ]


unstyledText : String -> List InlineBlock.StyledString
unstyledText body =
    [ { string = body, style = { isCode = False, isBold = False, isItalic = False, link = Nothing } } ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
