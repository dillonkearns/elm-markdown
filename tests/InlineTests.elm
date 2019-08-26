module InlineTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Inlines as Inlines
import Markdown.Parser exposing (..)
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


suite : Test
suite =
    describe "inline parsing"
        [ test "code spans" <|
            \() ->
                """`code`"""
                    |> Advanced.run Inlines.parse
                    |> Expect.equal
                        (Ok
                            [ { string = "code"
                              , style =
                                    { isCode = True
                                    , isBold = False
                                    , isItalic = False
                                    , link = Nothing
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
                            [ Heading 1 (unstyledText "Heading")
                            , Html "div"
                                []
                                [ Heading 1 (unstyledText "Heading in a div!")
                                ]
                            ]
                        )
        , test "simple link" <|
            \() ->
                """[Contact](/contact)
"""
                    |> Advanced.run multiParser
                    |> Expect.equal
                        (Ok
                            [ Body
                                [ { string = "Contact"
                                  , style =
                                        { isCode = False
                                        , isBold = False
                                        , isItalic = False
                                        , link =
                                            Just
                                                { destination = "/contact"
                                                , title = Nothing
                                                }
                                        }
                                  }
                                ]
                            ]
                        )
        ]


unstyledText : String -> List Inlines.StyledString
unstyledText body =
    [ { string = body
      , style =
            { isCode = False
            , isBold = False
            , isItalic = False
            , link = Nothing
            }
      }
    ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
