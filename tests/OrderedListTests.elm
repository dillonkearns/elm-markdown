module OrderedListTests exposing (..)

import Expect exposing (Expectation)
import Markdown.OrderedList
import Markdown.Parser exposing (..)
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


suite : Test
suite =
    describe "list parsing"
        [ test "basic list with that start with '1.'" <|
            \() ->
                """1. Item 1
2. Item 2
3. Item 3
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.equal
                        (Ok
                            ( 1
                            , [ "Item 1"
                              , "Item 2"
                              , "Item 3"
                              ]
                            )
                        )
        , test "basic list with that start with '1.' and repeat the same starting number" <|
            \() ->
                """1. Item 1
1. Item 2
1. Item 3
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.equal
                        (Ok
                            ( 1
                            , [ "Item 1"
                              , "Item 2"
                              , "Item 3"
                              ]
                            )
                        )
        , test "basic list with that start with '1.' and follow with random numbers" <|
            \() ->
                """1. Item 1
3. Item 2
8. Item 3
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.equal
                        (Ok
                            ( 1
                            , [ "Item 1"
                              , "Item 2"
                              , "Item 3"
                              ]
                            )
                        )
        , test "basic list with that start with '3.' and follow with the same number" <|
            \() ->
                """3. Item 1
3. Item 2
3. Item 3
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.equal
                        (Ok
                            ( 3
                            , [ "Item 1"
                              , "Item 2"
                              , "Item 3"
                              ]
                            )
                        )
        , test "A list can start at 0" <|
            \() ->
                """0. Item 1
1. Item 2
2. Item 3
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.equal
                        (Ok
                            ( 0
                            , [ "Item 1"
                              , "Item 2"
                              , "Item 3"
                              ]
                            )
                        )
        , test "A list starting number can have leading zeros" <|
            \() ->
                """003. Item 1
0003. Item 2
00003. Item 3
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.equal
                        (Ok
                            ( 3
                            , [ "Item 1"
                              , "Item 2"
                              , "Item 3"
                              ]
                            )
                        )
        , test "basic list with that start with '3.' and follow with decreasing numbers" <|
            \() ->
                """3. Item 1
2. Item 2
1. Item 3
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.equal
                        (Ok
                            ( 3
                            , [ "Item 1"
                              , "Item 2"
                              , "Item 3"
                              ]
                            )
                        )
        , test "basic list with '1)'" <|
            \() ->
                """1) Item 1
2) Item 2
3) Item 3
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.equal
                        (Ok
                            ( 1
                            , [ "Item 1"
                              , "Item 2"
                              , "Item 3"
                              ]
                            )
                        )
        , test "basic list with that start with '3)' and follow with the same number" <|
            \() ->
                """3) Item 1
3) Item 2
3) Item 3
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.equal
                        (Ok
                            ( 3
                            , [ "Item 1"
                              , "Item 2"
                              , "Item 3"
                              ]
                            )
                        )
        , test "sibling lists with different markers" <|
            \() ->
                """1) Item 1
2) Item 2
3) Item 3
1. Item 4
2. Item 5
3. Item 6
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.equal
                        (Ok
                            ( 1
                            , [ "Item 1"
                              , "Item 2"
                              , "Item 3"
                              ]
                            )
                        )
        , test "When the marker changes in the middle of a list" <|
            \() ->
                """1. foo
2. bar
3) baz
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.equal
                        (Ok
                            ( 1
                            , [ "foo"
                              , "bar"
                              ]
                            )
                        )
        , test "Text starting with a decimal number" <|
            \() ->
                """4.3
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.err
        , test "A list that doesn't have a space after the period marker" <|
            \() ->
                """1.testing
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.err
        , test "A list that doesn't have a space after the parenthesis marker" <|
            \() ->
                """1)testing
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.err
        , test "Text starting with a parenthetical statement" <|
            \() ->
                """(test) data
"""
                    |> Advanced.run Markdown.OrderedList.parser
                    |> Expect.err
        ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
