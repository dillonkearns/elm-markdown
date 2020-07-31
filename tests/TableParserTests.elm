module TableParserTests exposing (delimiterParsingSuite, fullTableSuite, rowParsingSuite)

import Expect exposing (Expectation)
import Markdown.Table
import Markdown.TableParser as TableParser exposing (..)
import Parser
import Parser.Advanced as Advanced exposing (..)
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


delimiterParsingSuite : Test
delimiterParsingSuite =
    describe "delimiter row"
        [ test "single with pipes" <|
            \() ->
                "|---|"
                    |> Advanced.run delimiterRowParser
                    |> Expect.equal
                        (Ok (DelimiterRow 1))
        , test "two columns" <|
            \() ->
                "|--|--|"
                    |> Advanced.run delimiterRowParser
                    |> Expect.equal
                        (Ok (DelimiterRow 2))
        , test "no leading" <|
            \() ->
                "--|--|"
                    |> Advanced.run delimiterRowParser
                    |> Expect.equal
                        (Ok (DelimiterRow 2))
        , test "no trailing" <|
            \() ->
                "|--|--"
                    |> Advanced.run delimiterRowParser
                    |> Expect.equal
                        (Ok (DelimiterRow 2))
        , test "no leading or trailing" <|
            \() ->
                "--|--"
                    |> Advanced.run delimiterRowParser
                    |> Expect.equal
                        (Ok (DelimiterRow 2))
        , test "delimiter row with no trailing or leading pipes" <|
            \() ->
                -- TODO should this be an error?
                "--"
                    |> Advanced.run delimiterRowParser
                    |> Expect.equal
                        (Ok (DelimiterRow 1))
        , test "delimiter row with space padding" <|
            \() ->
                "| -- |-- | --   |"
                    |> Advanced.run delimiterRowParser
                    |> Expect.equal
                        (Ok (DelimiterRow 3))
        , test "delimiter row with space padding and no leading" <|
            \() ->
                "-- |-- | --   |"
                    |> Advanced.run delimiterRowParser
                    |> Expect.equal
                        (Ok (DelimiterRow 3))
        , test "delimiter row with space padding and no trailing" <|
            \() ->
                "| -- |-- | --   "
                    |> Advanced.run delimiterRowParser
                    |> Expect.equal
                        (Ok (DelimiterRow 3))
        , test "delimiter rows cannot have spaces between the hyphens" <|
            \() ->
                "|---|- -|"
                    |> expectParserFail delimiterRowParser
        ]


rowParsingSuite : Test
rowParsingSuite =
    describe "row parser"
        [ test "parse row" <|
            \() ->
                "| abc | def |"
                    |> Advanced.run rowParser
                    |> Expect.equal
                        (Ok
                            [ "abc", "def" ]
                        )
        , test "single row" <|
            \() ->
                "| abc |"
                    |> Advanced.run rowParser
                    |> Expect.equal
                        (Ok
                            [ "abc" ]
                        )
        , test "row without trailing or leading pipes" <|
            \() ->
                "cell 1 | cell 2 | cell 3"
                    |> Advanced.run rowParser
                    |> Expect.equal
                        (Ok
                            [ "cell 1", "cell 2", "cell 3" ]
                        )
        ]


fullTableSuite : Test
fullTableSuite =
    describe "GFM tables"
        [ test "simple case" <|
            \() ->
                """| abc | def |
|---|---|"""
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                , { label = "def", alignment = Nothing }
                                ]
                                []
                            )
                        )
        , test "simple case with trailing whitespace" <|
            \() ->
                """| abc | def |
|---|---|
"""
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                , { label = "def", alignment = Nothing }
                                ]
                                []
                            )
                        )
        , test "The delimiter row cannot have fewer columns than the header" <|
            \() ->
                """| abc | def |
|---|
"""
                    |> expectFail
        , test "The delimiter row cannot have more columns than the header" <|
            \() ->
                """| abc | def |
|---|--|--|
"""
                    |> expectFail
        , test "tables must have at least one delimiter" <|
            \() ->
                """| abc | def |
|"""
                    |> expectFail
        , test "table must have a delimiter row" <|
            \() ->
                """| abc | def |

Hey, I forgot to finish my table! Whoops!
                           """
                    |> expectFail
        , test "table must have a delimiter row before body rows" <|
            \() ->
                """| abc | def |
| foo | bar |
| bar | baz |
"""
                    |> expectFail
        , test "tables have data rows" <|
            \() ->
                """| abc | def |
| --- | --- |
| foo | bar |
| bar | baz |
"""
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                , { label = "def", alignment = Nothing }
                                ]
                                [ [ "foo", "bar" ]
                                , [ "bar", "baz" ]
                                ]
                            )
                        )
        , test "the data rows can have varying length but the result should be even" <|
            \() ->
                """| abc | def |
| --- | --- |
| bar |
| bar | baz | boo |
"""
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                , { label = "def", alignment = Nothing }
                                ]
                                [ [ "bar", "" ]
                                , [ "bar", "baz" ]
                                ]
                            )
                        )
        ]



-- NOTABLE TODOS
-- handle \| appropriately
-- handle


expectParserFail someParser input =
    case Advanced.run someParser input of
        Ok _ ->
            Expect.fail "Expected a parser error."

        Err _ ->
            Expect.pass


expectFail input =
    case Advanced.run parser input of
        Ok _ ->
            Expect.fail "Expected a parser error."

        Err _ ->
            Expect.pass
