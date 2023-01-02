module OrderedListTests exposing (suite)

import Expect
import Markdown.Block exposing (Block(..), Inline(..))
import Markdown.OrderedList
import Markdown.Parser
import Parser.Advanced as Advanced
import Test exposing (..)


suite : Test
suite =
    describe "list parsing"
        [ test "basic list with that start with '1.'" <|
            \() ->
                """1. Item 1
2. Item 2
3. Item 3
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ OrderedList Markdown.Block.Tight
                                1
                                [ [ Paragraph [ Text "Item 1" ] ]
                                , [ Paragraph [ Text "Item 2" ] ]
                                , [ Paragraph [ Text "Item 3" ] ]
                                ]
                            ]
                        )
        , test "list ending without newline" <|
            \() ->
                """1. Item 1
2. Item 2
3. Item 3"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ OrderedList Markdown.Block.Tight
                                1
                                [ [ Paragraph [ Text "Item 1" ] ]
                                , [ Paragraph [ Text "Item 2" ] ]
                                , [ Paragraph [ Text "Item 3" ] ]
                                ]
                            ]
                        )
        , test "basic list with that start with '1.' and repeat the same starting number" <|
            \() ->
                """1. Item 1
1. Item 2
1. Item 3
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ OrderedList Markdown.Block.Tight
                                1
                                [ [ Paragraph [ Text "Item 1" ] ]
                                , [ Paragraph [ Text "Item 2" ] ]
                                , [ Paragraph [ Text "Item 3" ] ]
                                ]
                            ]
                        )
        , test "basic list with that start with '3)' and follow with the same number" <|
            \() ->
                """3) Item 1
3) Item 2
3) Item 3
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ OrderedList Markdown.Block.Tight
                                3
                                [ [ Paragraph [ Text "Item 1" ] ]
                                , [ Paragraph [ Text "Item 2" ] ]
                                , [ Paragraph [ Text "Item 3" ] ]
                                ]
                            ]
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
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ OrderedList Markdown.Block.Tight
                                1
                                [ [ Paragraph [ Text "Item 1" ] ]
                                , [ Paragraph [ Text "Item 2" ] ]
                                , [ Paragraph [ Text "Item 3" ] ]
                                ]
                            , OrderedList Markdown.Block.Tight
                                1
                                [ [ Paragraph [ Text "Item 4" ] ]
                                , [ Paragraph [ Text "Item 5" ] ]
                                , [ Paragraph [ Text "Item 6" ] ]
                                ]
                            ]
                        )
        , test "When the marker changes in the middle of a list" <|
            \() ->
                """1. foo
2. bar
3) baz
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ OrderedList Markdown.Block.Tight
                                1
                                [ [ Paragraph [ Text "foo" ] ]
                                , [ Paragraph [ Text "bar" ] ]
                                ]
                            , OrderedList Markdown.Block.Tight
                                3
                                [ [ Paragraph [ Text "baz" ] ]
                                ]
                            ]
                        )
        , test "basic list with that start with '3.' and follow with the same number" <|
            \() ->
                """3. Item 1
3. Item 2
3. Item 3
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ OrderedList Markdown.Block.Tight
                                3
                                [ [ Paragraph [ Text "Item 1" ] ]
                                , [ Paragraph [ Text "Item 2" ] ]
                                , [ Paragraph [ Text "Item 3" ] ]
                                ]
                            ]
                        )
        , test "A list can start at 0" <|
            \() ->
                """0. Item 1
1. Item 2
2. Item 3
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ OrderedList Markdown.Block.Tight
                                0
                                [ [ Paragraph [ Text "Item 1" ] ]
                                , [ Paragraph [ Text "Item 2" ] ]
                                , [ Paragraph [ Text "Item 3" ] ]
                                ]
                            ]
                        )
        , test "A list starting number can have leading zeros" <|
            \() ->
                """003. Item 1
0003. Item 2
00003. Item 3
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ OrderedList Markdown.Block.Tight
                                3
                                [ [ Paragraph [ Text "Item 1" ] ]
                                , [ Paragraph [ Text "Item 2" ] ]
                                , [ Paragraph [ Text "Item 3" ] ]
                                ]
                            ]
                        )
        , test "basic list with that start with '3.' and follow with decreasing numbers" <|
            \() ->
                """3. Item 1
2. Item 2
1. Item 3
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ OrderedList Markdown.Block.Tight
                                3
                                [ [ Paragraph [ Text "Item 1" ] ]
                                , [ Paragraph [ Text "Item 2" ] ]
                                , [ Paragraph [ Text "Item 3" ] ]
                                ]
                            ]
                        )
        , test "basic list with '1)'" <|
            \() ->
                """1) Item 1
2) Item 2
3) Item 3
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ OrderedList Markdown.Block.Tight
                                1
                                [ [ Paragraph [ Text "Item 1" ] ]
                                , [ Paragraph [ Text "Item 2" ] ]
                                , [ Paragraph [ Text "Item 3" ] ]
                                ]
                            ]
                        )
        , test "When there is an empty item" <|
            \() ->
                """1. foo
2.
3. bar
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ OrderedList Markdown.Block.Tight
                                1
                                [ [ Paragraph [ Text "foo" ] ]
                                , []
                                , [ Paragraph [ Text "bar" ] ]
                                ]
                            ]
                        )
        , test "Text starting with a decimal number" <|
            \() ->
                """4.3
"""
                    |> Advanced.run (Markdown.OrderedList.parser False)
                    |> Expect.err
        , test "A list that doesn't have a space after the period marker" <|
            \() ->
                """1.testing
"""
                    |> Advanced.run (Markdown.OrderedList.parser False)
                    |> Expect.err
        , test "A list that doesn't have a space after the parenthesis marker" <|
            \() ->
                """1)testing
"""
                    |> Advanced.run (Markdown.OrderedList.parser False)
                    |> Expect.err
        , test "Text starting with a parenthetical statement" <|
            \() ->
                """(test) data
"""
                    |> Advanced.run (Markdown.OrderedList.parser False)
                    |> Expect.err
        , test "A list cannot start with a number longer than 9 digits" <|
            \() ->
                """1234567890. item 1
1234567891. item 2
1234567892. item 3
"""
                    |> Advanced.run (Markdown.OrderedList.parser False)
                    |> Expect.err
        ]
