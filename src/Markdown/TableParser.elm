module Markdown.TableParser exposing (..)

import Markdown.Table
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (oneOrMore, tokenHelp)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias Table =
    Markdown.Table.Table String


parser : Parser Table
parser =
    (Advanced.succeed (\headers delimiters body -> ( headers, delimiters, body ))
        |= rowParser
        |= delimiterRowParser
        |= bodyParser
    )
        |> Advanced.andThen
            (\( headers, DelimiterRow delimiterCount, body ) ->
                if List.length headers == delimiterCount then
                    Advanced.succeed
                        (Markdown.Table.Table
                            (headers
                                |> List.map
                                    (\headerCell ->
                                        { label = headerCell
                                        , alignment = Nothing
                                        }
                                    )
                            )
                            (standardizeRowLength (List.length headers) body)
                        )

                else
                    Advanced.problem (Parser.Problem "Tables must have the same number of header columns as delimiter columns")
            )


standardizeRowLength : Int -> List (List String) -> List (List String)
standardizeRowLength expectedLength =
    List.map
        (\row ->
            let
                rowLength =
                    List.length row
            in
            case compare expectedLength rowLength of
                LT ->
                    List.take expectedLength row

                EQ ->
                    row

                GT ->
                    row ++ List.repeat (expectedLength - rowLength) ""
        )


rowParser : Parser (List String)
rowParser =
    succeed
        (\rowString ->
            rowString
                |> dropTrailingPipe
                |> String.split "|"
                |> List.map String.trim
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
    loop 0 delimiterRowHelp
        |> andThen
            (\((DelimiterRow count) as delimiterRow) ->
                if count > 0 then
                    succeed delimiterRow

                else
                    problem (Parser.Expecting "Must have at least one column in delimiter row.")
            )


requirePipeIfNotFirst : Int -> Parser ()
requirePipeIfNotFirst found =
    if found > 0 then
        tokenHelp "|"

    else
        oneOf
            [ tokenHelp "|"
            , succeed ()
            ]


delimiterRowHelp : Int -> Parser (Step Int DelimiterRow)
delimiterRowHelp found =
    oneOf
        [ tokenHelp "|\n" |> Advanced.map (\_ -> Done (DelimiterRow found))
        , tokenHelp "\n" |> Advanced.map (\_ -> Done (DelimiterRow found))
        , Advanced.end (Parser.Expecting "end") |> Advanced.map (\_ -> Done (DelimiterRow found))
        , backtrackable (succeed (Done (DelimiterRow found)) |. tokenHelp "|" |. Advanced.end (Parser.Expecting "end"))
        , succeed (Loop (found + 1))
            |. requirePipeIfNotFirst found
            |. spaces
            |. oneOrMore (\c -> c == '-')
            |. spaces
        ]



--delimiterRowHelp : Int -> Parser (Step Int DelimiterRow)
--delimiterRowHelp found =
--    oneOf
--        [ succeed identity
--            |. tokenHelp "|"
--            |. spaces
--            |= oneOf
--                [ hyphens found
--                , Advanced.end (Parser.Expecting "end")
--                    |> map (\_ -> Done (DelimiterRow found))
--                , tokenHelp "\n"
--                    --|. chompIf (\c -> c == '\n') (Parser.Expecting "\\n")
--                    |> map (\_ -> Done (DelimiterRow found))
--                ]
--        , hyphens found
--        , tokenHelp "\n"
--            --|. chompIf (\c -> c == '\n') (Parser.Expecting "\\n")
--            |> map (\_ -> Done (DelimiterRow found))
--        , Advanced.end (Parser.Expecting "end")
--            |> map (\_ -> Done (DelimiterRow found))
--        ]


bodyParser : Parser (List (List String))
bodyParser =
    loop [] bodyParserHelp


bodyParserHelp : List (List String) -> Parser (Step (List (List String)) (List (List String)))
bodyParserHelp revRows =
    oneOf
        [ rowParser
            |> andThen
                (\row ->
                    if List.isEmpty row || List.all String.isEmpty row then
                        Advanced.problem (Parser.Problem "A line must have at least one column")

                    else
                        Advanced.succeed (Loop (row :: revRows))
                )
        , tokenHelp "\n"
            |> map (\_ -> Done (List.reverse revRows))
        , Advanced.end (Parser.Expecting "end")
            |> map (\_ -> Done (List.reverse revRows))
        ]


dropTrailingPipe : String -> String
dropTrailingPipe string =
    if string |> String.endsWith "|" then
        string
            |> String.dropRight 1

    else
        string
