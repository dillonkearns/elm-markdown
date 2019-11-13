module ListTests exposing (suite)

import Expect exposing (Expectation)
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
        [ test "basic list with '-'" <|
            \() ->
                """- Item 1
- Item 2
- Item 3
"""
                    |> Advanced.run Markdown.List.parser
                    |> Expect.equal
                        (Ok
                            [ "Item 1"
                            , "Item 2"
                            , "Item 3"
                            ]
                        )
        , test "basic list with '+'" <|
            \() ->
                """+ Item 1
+ Item 2
+ Item 3
"""
                    |> Advanced.run Markdown.List.parser
                    |> Expect.equal
                        (Ok
                            [ "Item 1"
                            , "Item 2"
                            , "Item 3"
                            ]
                        )
        , test "basic list with '*'" <|
            \() ->
                """* Item 1
* Item 2
* Item 3
"""
                    |> Advanced.run Markdown.List.parser
                    |> Expect.equal
                        (Ok
                            [ "Item 1"
                            , "Item 2"
                            , "Item 3"
                            ]
                        )
        , test "sibling lists with different markers" <|
            \() ->
                """- Item 1
- Item 2
- Item 3
+ Item 4
+ Item 5
+ Item 6
* Item 7
* Item 8
* Item 9
"""
                    |> Advanced.run Markdown.List.parser
                    |> Expect.equal
                        (Ok
                            [ "Item 1"
                            , "Item 2"
                            , "Item 3"
                            ]
                        )
        , test "A list item with emphasis in it and starting with '*'" <|
            \() ->
                """* Item 1 is *emphasized*
* Item 2
* Item 3
*emphasized text following the list*
"""
                    |> Advanced.run Markdown.List.parser
                    |> Expect.equal
                        (Ok
                            [ "Item 1 is *emphasized*"
                            , "Item 2"
                            , "Item 3"
                            ]
                        )
        ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
