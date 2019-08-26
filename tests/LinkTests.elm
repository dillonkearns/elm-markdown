module LinkTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Link
import Markdown.Parser exposing (..)
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
parse =
    Markdown.Parser.parse


suite : Test
suite =
    describe "link parsing"
        [ test "basic link with no title" <|
            \() ->
                """[About](/about)"""
                    |> Advanced.run Markdown.Link.parser
                    |> Expect.equal
                        (Ok
                            { description = "About"
                            , destination = "/about"
                            , title = Nothing
                            }
                        )
        ]


unstyledText body =
    [ { string = body, style = { isCode = False, isBold = False, isItalic = False } } ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
