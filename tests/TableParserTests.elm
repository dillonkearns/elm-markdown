module TableParserTests exposing (delimiterParsingSuite, fullTableSuite, rowParsingSuite)

import Expect exposing (Expectation)
import Markdown.Block exposing (Alignment(..))
import Markdown.Table exposing (TableDelimiterRow(..))
import Markdown.TableParser
import Parser.Advanced as Advanced
import Test exposing (Test, describe, test)


expectDelimiterRowOk : String -> List (Maybe Alignment) -> Expectation
expectDelimiterRowOk testString columns =
    testString
        |> Advanced.run Markdown.TableParser.delimiterRowParser
        |> Expect.equal
            (Ok (TableDelimiterRow { raw = testString, trimmed = String.trim testString } columns))


delimiterParsingSuite : Test
delimiterParsingSuite =
    describe "delimiter row"
        [ test "single column with pipes" <|
            \() ->
                expectDelimiterRowOk "|---|" [ Nothing ]
        , test "two columns" <|
            \() ->
                expectDelimiterRowOk "|--|--|" [ Nothing, Nothing ]
        , test "no leading" <|
            \() ->
                expectDelimiterRowOk "--|--|" [ Nothing, Nothing ]
        , test "no trailing" <|
            \() ->
                expectDelimiterRowOk "|--|--" [ Nothing, Nothing ]
        , test "no leading or trailing" <|
            \() ->
                expectDelimiterRowOk "--|--" [ Nothing, Nothing ]
        , test "only a single hyphen per column" <|
            \() ->
                expectDelimiterRowOk "- | -" [ Nothing, Nothing ]
        , test "delimiter row with no trailing or leading pipes" <|
            \() ->
                "--"
                    |> expectParserFail Markdown.TableParser.delimiterRowParser
        , test "delimiter row with space padding" <|
            \() ->
                expectDelimiterRowOk "| -- |-- | --   |" [ Nothing, Nothing, Nothing ]
        , test "delimiter row with space padding and no leading" <|
            \() ->
                expectDelimiterRowOk "-- |-- | --   |" [ Nothing, Nothing, Nothing ]
        , test "delimiter row with space padding and no trailing" <|
            \() ->
                expectDelimiterRowOk "| -- |-- | --   " [ Nothing, Nothing, Nothing ]
        , test "delimiter rows cannot have spaces between the hyphens" <|
            \() ->
                "|---|- -|"
                    |> expectParserFail Markdown.TableParser.delimiterRowParser
        , test "delimiter row with alignment in columns" <|
            \() ->
                expectDelimiterRowOk "| :-- |-- | :-: | ---:   " [ Just AlignLeft, Nothing, Just AlignCenter, Just AlignRight ]
        ]


rowParsingSuite : Test
rowParsingSuite =
    describe "row parser"
        [ test "simple row" <|
            \() ->
                "| abc | def |"
                    |> Advanced.run Markdown.TableParser.rowParser
                    |> Expect.equal
                        (Ok
                            [ "abc", "def" ]
                        )
        , test "single row" <|
            \() ->
                "| abc |"
                    |> Advanced.run Markdown.TableParser.rowParser
                    |> Expect.equal
                        (Ok
                            [ "abc" ]
                        )
        , test "row without trailing or leading pipes" <|
            \() ->
                "cell 1 | cell 2 | cell 3"
                    |> Advanced.run Markdown.TableParser.rowParser
                    |> Expect.equal
                        (Ok
                            [ "cell 1", "cell 2", "cell 3" ]
                        )
        , test "row with escaped pipes" <|
            \() ->
                "| abc | a \\| b |"
                    |> Advanced.run Markdown.TableParser.rowParser
                    |> Expect.equal
                        (Ok
                            [ "abc", "a | b" ]
                        )
        , test "row with escaped pipes at the end" <|
            \() ->
                "| abc | def\\|"
                    |> Advanced.run Markdown.TableParser.rowParser
                    |> Expect.equal
                        (Ok
                            [ "abc", "def|" ]
                        )
        , test "row with escaped pipes at the beginning" <|
            \() ->
                "\\|  abc | def |"
                    |> Advanced.run Markdown.TableParser.rowParser
                    |> Expect.equal
                        (Ok
                            [ "|  abc", "def" ]
                        )
        , test "row with empty cell contents" <|
            \() ->
                "| abc |  |"
                    |> Advanced.run Markdown.TableParser.rowParser
                    |> Expect.equal
                        (Ok
                            [ "abc", "" ]
                        )
        , test "with double escaped pipe character" <|
            \() ->
                "| abc \\\\|  |"
                    |> Advanced.run Markdown.TableParser.rowParser
                    |> Expect.equal
                        (Ok
                            [ "abc |" ]
                        )
        , test "with triple escaped pipe character" <|
            \() ->
                "| abc \\\\\\|  |"
                    |> Advanced.run Markdown.TableParser.rowParser
                    |> Expect.equal
                        (Ok
                            [ "abc \\|" ]
                        )
        ]


fullTableSuite : Test
fullTableSuite =
    describe "GFM tables"
        [ test "simple case" <|
            \() ->
                """| abc | def |
|---|---|"""
                    |> Advanced.run Markdown.TableParser.parser
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
                    |> Advanced.run Markdown.TableParser.parser
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
                    |> Advanced.run Markdown.TableParser.parser
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
                    |> Advanced.run Markdown.TableParser.parser
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
        , test "tables without surrounding pipes" <|
            \() ->
                """abc | def
--- | ---
foo | bar
bar | baz
"""
                    |> Advanced.run Markdown.TableParser.parser
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
        , test "tables without body and without surrounding pipes" <|
            \() ->
                """abc | def
--- | ---
"""
                    |> Advanced.run Markdown.TableParser.parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                , { label = "def", alignment = Nothing }
                                ]
                                []
                            )
                        )
        , test "tables with only a single column and the delimiter does NOT have surrounding pipes" <|
            \() ->
                """| abc |
---
bar

"""
                    |> expectFail
        , test "tables with only a single column and the delimiter has surrounding pipes" <|
            \() ->
                """| abc |
| --- |
bar
"""
                    |> Advanced.run Markdown.TableParser.parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                ]
                                [ [ "bar" ] ]
                            )
                        )
        , test "tables with only a single column and the delimiter has surrounding pipes but the header does not" <|
            \() ->
                """abc
| --- |
bar
"""
                    |> Advanced.run Markdown.TableParser.parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                ]
                                [ [ "bar" ] ]
                            )
                        )
        , test "table with many layers of escaping" <|
            \() ->
                """|abc|
|---|
|\\\\\\\\\\\\|
|\\\\\\\\\\|
|\\\\\\\\|
|\\\\\\|
|\\\\|
|\\|
|\\\\\\\\||

"""
                    |> Advanced.run Markdown.TableParser.parser
                    |> Expect.equal
                        (Ok
                            (Markdown.Table.Table
                                [ { label = "abc", alignment = Nothing }
                                ]
                                [ [ "\\\\|" ]
                                , [ "\\\\|" ]
                                , [ "\\|" ]
                                , [ "\\|" ]
                                , [ "|" ]
                                , [ "|" ]
                                , [ "\\|" ]
                                ]
                            )
                        )
        ]


expectParserFail : Advanced.Parser c x a -> String -> Expectation
expectParserFail someParser input =
    case Advanced.run someParser input of
        Ok _ ->
            Expect.fail "Expected a parser error."

        Err _ ->
            Expect.pass


expectFail : String -> Expectation
expectFail input =
    case Advanced.run Markdown.TableParser.parser input of
        Ok _ ->
            Expect.fail "Expected a parser error."

        Err _ ->
            Expect.pass
