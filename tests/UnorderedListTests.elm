module UnorderedListTests exposing (suite)

import Expect
import Markdown.Block exposing (Block(..), Inline(..), ListItem(..), ListSpacing(..), Task(..))
import Markdown.Parser
import Test exposing (..)


suite : Test
suite =
    describe "list parsing"
        [ test "basic list with '-'" <|
            \() ->
                """- Item 1
- Item 2
- Item 3
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ UnorderedList Tight
                                [ ListItem NoTask [ Paragraph [ Text "Item 1" ] ]
                                , ListItem NoTask [ Paragraph [ Text "Item 2" ] ]
                                , ListItem NoTask [ Paragraph [ Text "Item 3" ] ]
                                ]
                            ]
                        )
        , test "list that ends without newline" <|
            \() ->
                """- Item 1
- Item 2
- Item 3"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ UnorderedList Tight
                                [ ListItem NoTask [ Paragraph [ Text "Item 1" ] ]
                                , ListItem NoTask [ Paragraph [ Text "Item 2" ] ]
                                , ListItem NoTask [ Paragraph [ Text "Item 3" ] ]
                                ]
                            ]
                        )
        , test "basic list with '+'" <|
            \() ->
                """+ Item 1
+ Item 2
+ Item 3
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ UnorderedList Tight
                                [ ListItem NoTask [ Paragraph [ Text "Item 1" ] ]
                                , ListItem NoTask [ Paragraph [ Text "Item 2" ] ]
                                , ListItem NoTask [ Paragraph [ Text "Item 3" ] ]
                                ]
                            ]
                        )
        , test "basic list with '*'" <|
            \() ->
                """* Item 1
* Item 2
* Item 3
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ UnorderedList Tight
                                [ ListItem NoTask [ Paragraph [ Text "Item 1" ] ]
                                , ListItem NoTask [ Paragraph [ Text "Item 2" ] ]
                                , ListItem NoTask [ Paragraph [ Text "Item 3" ] ]
                                ]
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
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ UnorderedList Tight
                                [ ListItem NoTask [ Paragraph [ Text "Item 1" ] ]
                                , ListItem NoTask [ Paragraph [ Text "Item 2" ] ]
                                , ListItem NoTask [ Paragraph [ Text "Item 3" ] ]
                                ]
                            , UnorderedList Tight
                                [ ListItem NoTask [ Paragraph [ Text "Item 4" ] ]
                                , ListItem NoTask [ Paragraph [ Text "Item 5" ] ]
                                , ListItem NoTask [ Paragraph [ Text "Item 6" ] ]
                                ]
                            , UnorderedList Tight
                                [ ListItem NoTask [ Paragraph [ Text "Item 7" ] ]
                                , ListItem NoTask [ Paragraph [ Text "Item 8" ] ]
                                , ListItem NoTask [ Paragraph [ Text "Item 9" ] ]
                                ]
                            ]
                        )
        , test "A list item with emphasis in it and starting with '*'" <|
            \() ->
                """* Item 1 is *emphasized*
* Item 2
* Item 3
*emphasized text following the list*
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ UnorderedList Tight
                                [ ListItem NoTask
                                    [ Paragraph
                                        [ Text "Item 1 is "
                                        , Emphasis [ Text "emphasized" ]
                                        ]
                                    ]
                                , ListItem NoTask [ Paragraph [ Text "Item 2" ] ]
                                , ListItem NoTask
                                    [ Paragraph
                                        [ Text "Item 3\n"
                                        , Emphasis [ Text "emphasized text following the list" ]
                                        ]
                                    ]
                                ]
                            ]
                        )
        , test "When there is an empty item" <|
            \() ->
                """* foo
*
* bar
"""
                    |> Markdown.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ UnorderedList Tight
                                [ ListItem NoTask [ Paragraph [ Text "foo" ] ]
                                , ListItem NoTask []
                                , ListItem NoTask [ Paragraph [ Text "bar" ] ]
                                ]
                            ]
                        )
        ]
