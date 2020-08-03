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
            (\( headers, DelimiterRow delimiterCount _, body ) ->
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
    = DelimiterRow Int String


delimiterRowParser : Parser DelimiterRow
delimiterRowParser =
    mapChompedString (\delimiterText count -> DelimiterRow count (String.trim delimiterText)) (loop 0 delimiterRowHelp)
        |> andThen
            (\((DelimiterRow count delimiterText) as delimiterRow) ->
                if count == 1 && not (String.startsWith "|" delimiterText && String.endsWith "|" delimiterText) then
                    Advanced.problem (Parser.Problem "Tables with a single column must have pipes at the start and end of the delimiter row to avoid ambiguity.")

                else if count > 0 then
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


delimiterRowHelp : Int -> Parser (Step Int Int)
delimiterRowHelp found =
    oneOf
        [ backtrackable (tokenHelp "|\n" |> Advanced.map (\_ -> Done found))
        , tokenHelp "\n" |> Advanced.map (\_ -> Done found)
        , Advanced.end (Parser.Expecting "end") |> Advanced.map (\_ -> Done found)
        , backtrackable (succeed (Done found) |. tokenHelp "|" |. Advanced.end (Parser.Expecting "end"))
        , succeed (Loop (found + 1))
            |. requirePipeIfNotFirst found
            |. chompSpaceCharacter
            |. oneOrMore (\c -> c == '-')
            |. chompSpaceCharacter
        ]


chompSpaceCharacter : Parser ()
chompSpaceCharacter =
    chompWhile (\c -> c == ' ')


bodyParser : Parser (List (List String))
bodyParser =
    loop [] bodyParserHelp


bodyParserHelp : List (List String) -> Parser (Step (List (List String)) (List (List String)))
bodyParserHelp revRows =
    oneOf
        [ tokenHelp "\n"
            |> map (\_ -> Done (List.reverse revRows))
        , Advanced.end (Parser.Expecting "end")
            |> map (\_ -> Done (List.reverse revRows))
        , rowParser
            |> andThen
                (\row ->
                    if List.isEmpty row || List.all String.isEmpty row then
                        Advanced.problem (Parser.Problem "A line must have at least one column")

                    else
                        Advanced.succeed (Loop (row :: revRows))
                )
        ]


dropTrailingPipe : String -> String
dropTrailingPipe string =
    if string |> String.endsWith "|" then
        string
            |> String.dropRight 1

    else
        string
