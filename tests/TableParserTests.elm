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

        {-

           Possible strategies:
           * Do a second pass to validate it
           * During first pass, check

               TODO test case to fail parser immediately if there aren't enough
              | Col 1      | Col 2  | Col 3     |
              | ---------- | ------ |
              | **Cell 1** | Cell 2 | Extra col |
        -}
        ]


expectParserFail input someParser =
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
