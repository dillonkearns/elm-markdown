module LinkTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Block as Block exposing (Block)
import Markdown.Link as Link
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
                    |> Advanced.run Link.parser
                    |> Expect.equal
                        (Ok
                            (Link.Link
                                { description = "About"
                                , destination = "/about"
                                , title = Nothing
                                }
                            )
                        )
        , test "link with empty pointy brackets (gfm 496)" <|
            \() ->
                """[About](<>)"""
                    |> Advanced.run Link.parser
                    |> Expect.equal
                        (Ok
                            (Link.Link
                                { description = "About"
                                , destination = ""
                                , title = Nothing
                                }
                            )
                        )
        , test "link with space" <|
            \() ->
                """[About](/about me)"""
                    |> parserError
        , test "image" <|
            \() ->
                """![About](/my-image.jpg)"""
                    |> Advanced.run Link.parser
                    |> Expect.equal
                        (Ok
                            (Link.Image
                                { alt = "About"
                                , src = "/my-image.jpg"
                                }
                            )
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
