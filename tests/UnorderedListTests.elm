module UnorderedListTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.ListItem as ListItem exposing (ListItem)
import Markdown.Parser
import Markdown.UnorderedList
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
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1"
                            , plainItem "Item 2"
                            , plainItem "Item 3"
                            ]
                        )
        , test "list that ends without newline" <|
            \() ->
                """- Item 1
- Item 2
- Item 3"""
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1"
                            , plainItem "Item 2"
                            , plainItem "Item 3"
                            ]
                        )
        , test "basic list with '+'" <|
            \() ->
                """+ Item 1
+ Item 2
+ Item 3
"""
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1"
                            , plainItem "Item 2"
                            , plainItem "Item 3"
                            ]
                        )
        , test "basic list with '*'" <|
            \() ->
                """* Item 1
* Item 2
* Item 3
"""
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1"
                            , plainItem "Item 2"
                            , plainItem "Item 3"
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
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1"
                            , plainItem "Item 2"
                            , plainItem "Item 3"
                            ]
                        )
        , test "A list item with emphasis in it and starting with '*'" <|
            \() ->
                """* Item 1 is *emphasized*
* Item 2
* Item 3
*emphasized text following the list*
"""
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "Item 1 is *emphasized*"
                            , plainItem "Item 2"
                            , plainItem "Item 3"
                            ]
                        )
        , test "When there is an empty item" <|
            \() ->
                """* foo
*
* bar
"""
                    |> Advanced.run Markdown.UnorderedList.parser
                    |> Expect.equal
                        (Ok
                            [ plainItem "foo"
                            , plainItem ""
                            , plainItem "bar"
                            ]
                        )
        ]


plainItem : String -> ListItem
plainItem body =
    ListItem.PlainItem body
