module TableParserTests exposing (suite)

import Expect exposing (Expectation)
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (oneOrMore, tokenHelp)
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias MyParser a =
    Advanced.Parser String Parser.Problem a


type alias Row =
    List String


parser : Parser Table
parser =
    Advanced.succeed
        (\header ->
            Table
                header
                []
        )
        |= rowParser
        |. (delimiterRowParser |> map (Debug.log "delimiter"))


rowParser : MyParser (List String)
rowParser =
    succeed
        (\rowString ->
            rowString
                |> dropTrailingPipe
                |> String.split "|"
        )
        |. oneOf
            [ tokenHelp "|"
            , succeed ()
            ]
        |= Advanced.getChompedString
            (Advanced.chompUntilEndOr "\n")
        |. oneOf
            [ Advanced.end (Parser.Expecting "end")
            , chompIf (\c -> c == '\n') (Parser.Expecting "\\n")
            ]


type DelimiterRow
    = DelimiterRow Int


delimiterRowParser : Parser DelimiterRow
delimiterRowParser =
    loop 0 statementsHelp


statementsHelp : Int -> Parser (Step Int DelimiterRow)
statementsHelp found =
    oneOf
        [ succeed identity
            |. tokenHelp "|"
            |= oneOf
                [ hyphens found
                , Advanced.end (Parser.Expecting "end")
                    |> map (\_ -> Done (DelimiterRow found))
                ]
        , hyphens found
        , Advanced.end (Parser.Expecting "end")
            |> map (\_ -> Done (DelimiterRow found))
        ]


hyphens found =
    oneOrMore (\c -> c == '-')
        |> map (\_ -> Loop (found + 1))


dropTrailingPipe : String -> String
dropTrailingPipe string =
    if string |> String.endsWith "|" then
        string
            |> String.dropRight 1

    else
        string


type Table
    = Table Row (List Row)


suite : Test
suite =
    describe "GFM tables"
        [ test "simple case" <|
            \() ->
                """| abc | def |
|---|---|"""
                    |> Advanced.run parser
                    |> Expect.equal
                        (Ok
                            (Table
                                [ " abc ", " def " ]
                                []
                            )
                        )
        , test "table must have a delimiter row" <|
            \() ->
                """| abc | def |

Hey, I forgot to finish my table! Whoops!
                           """
                    |> expectFail
        , describe "delimiter row"
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
        , describe "row parser"
            [ test "parse row" <|
                \() ->
                    "| abc | def |"
                        |> Advanced.run rowParser
                        |> Expect.equal
                            (Ok
                                [ " abc ", " def " ]
                            )
            , test "single row" <|
                \() ->
                    "| abc |"
                        |> Advanced.run rowParser
                        |> Expect.equal
                            (Ok
                                [ " abc " ]
                            )
            , test "row without trailing or leading pipes" <|
                \() ->
                    "cell 1 | cell 2 | cell 3"
                        |> Advanced.run rowParser
                        |> Expect.equal
                            (Ok
                                [ "cell 1 ", " cell 2 ", " cell 3" ]
                            )
            ]

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
