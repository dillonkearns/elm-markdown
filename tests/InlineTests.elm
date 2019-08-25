module InlineTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Inlines
import Markdown.Parser exposing (..)
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) Block
parse input =
    Advanced.run lineParser input


suite : Test
suite =
    describe "inline parsing"
        [ test "code spans" <|
            \() ->
                """`code`"""
                    |> Advanced.run Markdown.Inlines.parse
                    |> Expect.equal
                        (Ok
                            [ { string = "code"
                              , style =
                                    { isCode = True
                                    , isBold = False
                                    , isItalic = False
                                    }
                              }
                            ]
                        )
        , test "heading within HTML" <|
            \() ->
                """# Heading
<div>
# Heading in a div!

</div>
"""
                    |> Advanced.run multiParser
                    |> Expect.equal
                        (Ok
                            [ Heading 1 "Heading"
                            , Html "div"
                                []
                                [ Body (unstyledText "")
                                , Heading 1 "Heading in a div!"
                                , Body (unstyledText "")
                                ]
                            ]
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
